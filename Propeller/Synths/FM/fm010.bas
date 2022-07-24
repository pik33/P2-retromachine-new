'-----------------------------------------------------------------------
' Frequency modulation sound toy
' v. 0.04 - 20220711
' pik33@o2.pl
'-----------------------------------------------------------------------

'sample rate=82264.77587890625
const HEAPSIZE = 32768
#include "retromachine.bi"


const version$="Propeller 2 FM synth v.0.08"


const c212=1.05946309435929526456
const c_1=8.17579891564370733371
const freqv=52209.06822*2 /16 '(*2 for debug until optimized, sampling at 41k) /16 for shl4
'const adsrv=52209 ' for 1 s
const maxchannel=15

'---- These are controller definitions for a Novation Impulse 61 MIDI controller keyboard ----------
'---- REDEFINE THEM FOR YOUR MIDI CONTROLLER KEYBOARD HERE -----------------------------------------

'    MIDI ctrl#		  ctrl name			ctrl function
const control00=$29  	' Novation Impulse slider #1, 	operator envelope rate 1
const control01=$2A	' Novation Impulse slider #2,	operator envelope rate 2
const control02=$2B	' Novation Impulse slider #3,  	operator envelope rate 3
const control03=$2C	' Novation Impulse slider #4,	operator envelope rate 4
const control04=$2D	' Novation Impulse slider #5,	operator envelope level 1
const control05=$2E	' Novation Impulse slider #6,	operator envelope level 2
const control06=$2F	' Novation Impulse slider #7,	operator envelope level 3
const control07=$30	' Novation Impulse slider #8, 	operator envelope level 4
const control08=$31	' Novation Impulse slider #9,	operator volume
const control09=$15	' Novation Impulse knob #1, 	operator feedback
const control10=$16	' Novation Impulse knob #2,
const control11=$17	' Novation Impulse knob #3,
const control12=$18	' Novation Impulse knob #4,
const control13=$19	' Novation Impulse knob #5,
const control14=$1a	' Novation Impulse knob #6,
const control15=$1b	' Novation Impulse knob #7,
const control16=$1c	' Novation Impulse knob #8,
const control17=$33	' Novation Impulse button #1,	
const control18=$34	' Novation Impulse button #2, 
const control19=$35	' Novation Impulse button #3,
const control20=$36	' Novation Impulse button #4,
const control21=$37	' Novation Impulse button #5,
const control22=$38	' Novation Impulse button #6,
const control23=$39	' Novation Impulse button #7,
const control24=$3a	' Novation Impulse button #8,
const control25=$3b	' Novation Impulse button #9,

'--------------------------------------------------------------------------------------------------

dim names(128,15) as ubyte
dim notes(127) as single 
dim oct(7)
dim r1b,r2b,r3b,r4b,l1b,l2b,l3b,l4b,lvb,senseb,rtscb,ldptb,rdptb,lcurb,rcurb,bkptb,freqcb,freqfb,freqmb,m0b,m1b,m2b,m3b,m4b,m5b,amsnb,fmsnb,onoffb as ubyte(5)  ' these are 20 bytes per op, midi control values! 
dim r1l,r2l,r3l,r4l,l1l,l2l,l3l,l4l,lvl,sensel,rtscl,ldptl,rdptl,lcurl,rcurl,bkptl,freqcl,freqfl,freqml,m0l,m1l,m2l,m3l,m4l,m5l,amsnl,fmsnl,onoffl as ulong(7)  ' these are real op values 
dim shared notes$(11)={"C","C#","D","D#","E","F","F#","G","G#","A","A#","H"}
dim freq1 as ulong
'These will be passed to the driver:
' r1l,r2l,r3l,r4l,l1l,l2l,l3l,l4l,lvl,amsn (done)
' rtscl,fmsnl todo 
' These are set from the main cog
',ldptl,rdptl,lcurl,rcurl,bkptl,freqcl,freqfl,freqml,onoffl 
 
 
dim ffreq as single(7)
dim operator, kbdpressed, base2, change as ulong
dim channelassign(31)
dim channelnotes(31)
dim midi as ulong
declare midibytes alias midi as ubyte(3)
dim flashbufl as ulong(1023)
declare flashbufb alias flashbufl as ubyte(4095)
dim ctrlstate

' ----------- start cogs'

flash.start(2000) ' presets at $880000 Preset is 96 longs, add reserved to 128 longs=512 bytes=8 presets per page
startpsram()
startmidi()
startvideo()
startaudio()

' To do: add a file system. This will need to make a display list, to reduce video ram usage. Or maybe something else... or go PSRAM way.

' ----------- init variables

for i=0 to 5 

  r1b(i)=127 : r1l(i)=round(61*exp(127.0/10.0))  				' envelope rate 1
  r2b(i)=127 : r2l(i)=round(61*exp(127.0/10.0))  				' envelope rate 2
  r3b(i)=127 : r3l(i)=round(61*exp(127.0/10.0))					' envelope rate 3
  r4b(i)=127 : r4l(i)=round(61*exp(127.0/10.0)) 				' envelope rate 4

  l1b(i)=127 : l1l(i)=$43FF_FFFF-$1C+$78F1E4*127  				' envelope level 1
  l2b(i)=127 : l2l(i)=$43FF_FFFF-$1C+$78F1E4*127				' envelope level 2
  l3b(i)=127 : l3l(i)=$43FF_FFFF-$1C+$78F1E4*127  				' envelope level 3
  l4b(i)=0   : l4l(i)=$43FF_FFFF-$1C+0						' envelope level 4
  
  if i=0 then lvb(i)=127 : lvl(i)=$3FFF else lvb(i)=0 : lvl(i)=0        	' operator output level
  if i=0 then senseb(i)=127 : sensel(i)=127 else senseb(i)=0 : sensel(i)=0  	' keyboard velocity sense
  rtscb(i)=0 :   rtscl(i)=0            						' rate scaling
  ldptb(i)=0 :   ldptl(i)=0	     						' left depth
  rdptb(i)=0 :   rdptl(i)=0	      						' right depth
  lcurb(i)=0 :   lcurl(i)=0							' left curve
  rcurb(i)=0 :   rcurl(i)=0							' right curve
  bkptb(i)=$3C : bkptl(i)=$3C							' breakpoint, default at C4
  freqcb(i)=3 :  freqcl(i)=3							' =1.0
  freqfb(i)=0 :  freqfl(i)=0							'
  freqmb(i)=0 :  freqml(i)=0							' frequency mode, rate or constant	
  m0b(i)=0 : 	 m0l(i)=0							' modulation level from Operator #0
  m1b(i)=0 : 	 m1l(i)=0							' modulation level from Operator #1
  m2b(i)=0 : 	 m2l(i)=0							' modulation level from Operator #2
  m3b(i)=0 : 	 m3l(i)=0							' modulation level from Operator #3
  m4b(i)=0 : 	 m4l(i)=0							' modulation level from Operator #4
  m5b(i)=0 :  	 m5l(i)=0							' modulation level from Operator #5
  amsnb(i)=0 :   amsnl(i)=0							' LFO amplitude modulation sensitivity
  fmsnb(i)=0 :   fmsnl(i)=0							' LFO frequency modulation sensitivity
  onoffb(i)=1 :  onoffl(i)=1							' operator on/off
  ffreq(i)=1
next i

operator=0
var f#=c_1: for i=0 to 127: notes(i)=f# : f#=f#*c212: next i
for i=0 to maxchannel: channelassign(i)=0 : next i
kbdpressed=1
ctrlstate=0


' ------------ init channel data
 
for i=0 to 15 : for j=0 to 140 step 4 : lpoke 144*i+base+j,0 : next j: next i

lpoke 144*i+base+08,%1111_11111111_11111111 ' these are ADSR skip values, 1=idle
lpoke 144*i+base+24,%1111_11111111_11111111
lpoke 144*i+base+40,%1111_11111111_11111111 
lpoke 144*i+base+56,%1111_11111111_11111111 
lpoke 144*i+base+72,%1111_11111111_11111111 
lpoke 144*i+base+88,%1111_11111111_11111111 


for i=0 to 5
  lpoke 2304+72*i+base+00,r1l(i)
  lpoke 2304+72*i+base+04,r2l(i)
  lpoke 2304+72*i+base+08,r3l(i)
  lpoke 2304+72*i+base+12,r4l(i)
  lpoke 2304+72*i+base+16,l1l(i)
  lpoke 2304+72*i+base+20,l2l(i)
  lpoke 2304+72*i+base+24,l3l(i)
  lpoke 2304+72*i+base+28,l4l(i)
  lpoke 2304+72*i+base+32,m0l(i)
  lpoke 2304+72*i+base+36,m1l(i)
  lpoke 2304+72*i+base+40,m2l(i)
  lpoke 2304+72*i+base+44,m3l(i)
  lpoke 2304+72*i+base+48,m4l(i)
  lpoke 2304+72*i+base+52,m5l(i)
  lpoke 2304+72*i+base+56,lvl(i)
  lpoke 2304+72*i+base+60,amsnl(i)
  lpoke 2304+72*i+base+64,fmsnl(i)
  lpoke 2304+72*i+base+68,rtscl(i)
next i
lpoke base+2160+96, lpeek(base+2160+96) + $20000000  ' tell the driver to read these data

'------- prepare the UI

preparepanels 
kbddraw(60,460)
v.outtextxycg(8,432,"Control mode 1 - rates, levels, mods, frequency           ",15,82)
waitms(1000)

'---------------------------------------------------------------------------------------
'------------------------------- THE MAIN LOOP -----------------------------------------
'---------------------------------------------------------------------------------------

90 

midi=rm.readmidi()
if midi<>0 then position 2,35 : v.write ("Midi command: "): v.write(v.inttohex(midi,8)) 
position 2*30,35: v.write("Channel time: "): v.write(v.inttostr2(lpeek($70),3)): v.write(" "): v.write(v.inttohex(lpeek(base+00),8)): v.write(" "):v.write(v.inttohex(lpeek(base+04),8)):v.write(" "):v.write(v.inttohex(lpeek(base+08),8))
if midi=0 then goto 90
let b3=midibytes(3): let b0=midibytes(0): let b1=midibytes(1) : let b2=midibytes(2)


'----- MIDI control change
' if ctrlstate=0 , sliders=r0-l4+lv, knobs=mod0-6+freq
' if ctrlstate=1  knobs=sen, rtsc, ldpt, rdpt, lcur, rcur, bkpt, freq mode, sliders=amsn,fmsn,freq mode
' if ctrlstate=2, preset load/save
' button 1-8 selects operator/lfo/set, button 9 changes ctrl state

if b3=$B0 then 

  if b1=control25 then 
    ctrlstate=ctrlstate+1
    if ctrlstate=4 then ctrlstate=0         ' 0: r,l,lv,m,freq -- 1: amsn,fmsn,sense, rate sclael curves, freq mode --- 3 presets --- 4 freeze
    if ctrlstate=0 then v.outtextxycg(8,432,"Control mode 1 - rates, levels, mods, frequency           ",15,82)
    if ctrlstate=1 then v.outtextxycg(8,432,"Control mode 2 - sense, rate scale, curves, LFO mod sense ",15,82)
    if ctrlstate=2 then v.outtextxycg(8,432,"Control mode 3 - presets - press a key to start           ",15,82)
    if ctrlstate=3 then v.outtextxycg(8,432,"Control mode 4 - freeze                                   ",15,82)
    if ctrlstate=4 then v.outtextxycg(8,432,"Control mode 5 - preeset name                             ",15,82)
    refreshpanel(operator,97,ctrlstate)
  endif  
  



  if ctrlstate=0 then

    if b1=control00 then r1b(operator)=b0 : r1l(operator)=round(61*exp(b0/10.0)) : lpoke 2304+72*operator+base+00,r1l(operator) : refreshpanel(operator,0,b0)	' rate 1 at slider 1
    if b1=control01 then r2b(operator)=b0 : r2l(operator)=round(61*exp(b0/10.0)) : lpoke 2304+72*operator+base+04,r2l(operator) : refreshpanel(operator,1,b0)	' rate 2 at slider 2
    if b1=control02 then r3b(operator)=b0 : r3l(operator)=round(61*exp(b0/10.0)) : lpoke 2304+72*operator+base+08,r3l(operator) : refreshpanel(operator,2,b0)	' rate 3 at slider 3
    if b1=control03 then r4b(operator)=b0 : r4l(operator)=round(61*exp(b0/10.0)) : lpoke 2304+72*operator+base+12,r4l(operator) : refreshpanel(operator,3,b0)	' rate 4 st slider 4
    if b1=control04 then l1b(operator)=b0 : l1l(operator)=$43FF_FFFF-$1C+$78F1E4*b0 : lpoke 2304+72*operator+base+16,l1l(operator) : refreshpanel(operator,4,b0)	' level 1 at slider 5
    if b1=control05 then l2b(operator)=b0 : l2l(operator)=$43FF_FFFF-$1C+$78F1E4*b0 : lpoke 2304+72*operator+base+20,l2l(operator) : refreshpanel(operator,5,b0)	' level 2 at slider 6
    if b1=control06 then l3b(operator)=b0 : l3l(operator)=$43FF_FFFF-$1C+$78F1E4*b0 : lpoke 2304+72*operator+base+24,l3l(operator) : refreshpanel(operator,6,b0)	' level 3 at slider 7
    if b1=control07 then l4b(operator)=b0 : l4l(operator)=$43FF_FFFF-$1C+$78F1E4*b0 : lpoke 2304+72*operator+base+28,l4l(operator) : refreshpanel(operator,7,b0)	' level 4 at slider 8
    if b1=control08 then lvb(operator)=b0 : lvl(operator)=round(exp(b0/11.4514))/4  : lpoke 2304+72*operator+base+56,lvl(operator) : refreshpanel(operator,8,b0)  ' Operator output level   

    if b1=control09 then m0b(operator)=b0 : m0l(operator)=round(exp(b0/11.4514)) : lpoke 2304+72*operator+base+32,m0l(operator) : refreshpanel(operator,20,b0)	' Modulation level from Operator 0
    if b1=control10 then m1b(operator)=b0 : m1l(operator)=round(exp(b0/11.4514)) : lpoke 2304+72*operator+base+36,m1l(operator) : refreshpanel(operator,21,b0)	' Modulation level from Operator 1
    if b1=control11 then m2b(operator)=b0 : m2l(operator)=round(exp(b0/11.4514)) : lpoke 2304+72*operator+base+40,m2l(operator) : refreshpanel(operator,22,b0)	' Modulation level from Operator 2
    if b1=control12 then m3b(operator)=b0 : m3l(operator)=round(exp(b0/11.4514)) : lpoke 2304+72*operator+base+44,m3l(operator) : refreshpanel(operator,23,b0)	' Modulation level from Operator 3
    if b1=control13 then m4b(operator)=b0 : m4l(operator)=round(exp(b0/11.4514)) : lpoke 2304+72*operator+base+48,m4l(operator) : refreshpanel(operator,24,b0)	' Modulation level from Operator 4
    if b1=control14 then m5b(operator)=b0 : m5l(operator)=round(exp(b0/11.4514)) : lpoke 2304+72*operator+base+52,m5l(operator) : refreshpanel(operator,25,b0)	' Modulation level from Operator 5
    if b1=control15 then 
      freqcb(operator)=b0
      if freqmb(operator)=0 then ffreq(operator)=((freqcb(operator)+1)/4.0)+freqfb(operator)/512.0 
      if freqmb(operator)=1 then let freq1=(freqcb(operator)/8)-2: ffreq(operator)=2.0^(freq1*1.0+freqfb(operator)/128.0)
      for i=0 to 15 : lpoke base+144*i+100+8*operator,round(notes(channelnotes(i))*ffreq(operator)*freqv) : next i         ' Frequency coarse level, serviced at the main cog
      refreshpanel(operator,16,b0)
      refreshpanel(operator,19,round(ffreq(operator)*1000000))
    endif  
    if b1=control16 then 
      freqfb(operator)=b0 
      if freqmb(operator)=0 then ffreq(operator)=((freqcb(operator)+1)/4.0)+freqfb(operator)/512.0 
      if freqmb(operator)=1 then let freq1=(freqcb(operator)/8)-2: ffreq(operator)=2.0^(freq1+freqfb(operator)/128.0)
      for i=0 to 15 : lpoke base+144*i+100+8*operator,round(notes(channelnotes(i))*ffreq(operator)*freqv) : next i         ' Frequency coarse level, serviced at the main cog
      refreshpanel(operator,17,b0)
      refreshpanel(operator,19,round(ffreq(operator)*1000000))
    endif  
  endif
 
 
  if ctrlstate=1 then

    if b1=control00 then amsnb(operator)=b0 : amsnl(operator)=round(exp(b0/11.4514)) : lpoke 2304+72*operator+base+60,amsnl(operator) : refreshpanel(operator,26,b0)	' LFO AM sensitivity at slider 1
    if b1=control01 then fmsnb(operator)=b0 : fmsnl(operator)=round(exp(b0/11.4514)) : lpoke 2304+72*operator+base+64,fmsnl(operator) : refreshpanel(operator,27,b0)	' LFO FM sensitivity at slider 2
 

    if b1=control09 then senseb(operator)=b0 : refreshpanel(operator,9,b0)												' KBD velocity sense
    if b1=control10 then rtscb(operator)=b0 : rtscl(operator)=round(exp(b0/11.4514)) : lpoke 2304+72*operator+base+368,rtscl(operator) : refreshpanel(operator,10,b0)	' Rate scaling
    if b1=control11 then ldptb(operator)=b0  : refreshpanel(operator,11,b0)												' Left depth
    if b1=control12 then rdptb(operator)=b0  : refreshpanel(operator,12,b0)												' Right depth
    if b1=control13 then lcurb(operator)=b0 mod 4 : refreshpanel(operator,13,b0)											' Left curve
    if b1=control14 then rcurb(operator)=b0 mod 4 : refreshpanel(operator,14,b0)											' Right curve
    if b1=control15 then bkptb(operator)=b0 : refreshpanel(operator,15,b0)												' Breakpoint
    if b1=control16 then 
      freqmb(operator)=b0 mod 2: refreshpanel(operator,18,b0)												' Breakpoint
      if freqmb(operator)=0 then ffreq(operator)=((freqcb(operator)+1)/4.0)+freqfb(operator)/512.0
      if freqmb(operator)=1 then let freq1=(freqcb(operator)/8)-2: ffreq(operator)=2.0^(freq1+freqfb(operator)/128.0)
      for i=0 to 15 : lpoke base+144*i+100+8*operator,round(notes(channelnotes(i))*ffreq(operator)*freqv) : next i         ' Frequency coarse level, serviced at the main cog
      refreshpanel(operator,16,b0)
  '   refreshpanel(operator,19,round(ffreq(operator)*1000000))
     endif  
  endif
  
  if ctrlstate=2 then  ' TODO - new preset save/load using bytes. 64 instead of 512
  
    do: midi=rm.readmidi() : loop until midibytes(3)= $90 orelse (midibytes(3)=$B0 andalso midibytes(1)=control25)
    if (midibytes(3)=$B0 andalso midibytes(1)=control25)then
      ctrlstate=3
      if ctrlstate=3 then v.outtextxycg(8,432,"Control mode 4 - freeze                                   ",15,82)
      goto 310
    endif
      
    v.outtextxycg(8,432,"Select the preset with a key                      ",15,82)
    do : midi=rm.readmidi() : loop until midibytes(3)=$90
    v.outtextxycg(8,432,"Preset "+v.inttostr2(midibytes(1),3)+" --- C2 load, C7 save, other exit",15,82) ' TODO name the preset
    let presetnum=midibytes(1)
    do : midi=rm.readmidi() : loop until midibytes(3)=$90    
    if midibytes(1)=$24 then ' C2, load a preset
       v.outtextxycg(8,432,"Press C4 to load a preset "+v.inttostr2(presetnum,3)+", any other key to cancel ",15,82)
      do : midi=rm.readmidi() : loop until midibytes(3)=$90
      if midibytes(1)=$3C then
        loadpreset(presetnum)
        v.box(8,432,1023,447,82): v.outtextxycg(8,432,"Preset "+v.inttostr2(presetnum,3)+" loaded ",15,82)
      else
        v.box(8,432,1023,447,82)
      endif   
    endif
    if midibytes(1)=$60 then ' C7, save a preset
       v.outtextxycg(8,432,"Press C4 to save a preset "+v.inttostr2(presetnum,3)+", any other key to cancel ",15,82)
       do : midi=rm.readmidi() : loop until midibytes(3)=$90
      if midibytes(1)=$3C then
        savepreset(presetnum)
        v.box(8,432,1023,447,82): v.outtextxycg(8,432,"Preset "+v.inttostr2(presetnum,3)+" saved ",15,82)
      else
        v.box(8,432,1023,447,82)
      endif   
    endif

  endif
  
310  
  if b1=control17 then let oldop=operator: operator=0: refreshpanel(operator,99,oldop) 
  if b1=control18 then let oldop=operator: operator=1: refreshpanel(operator,99,oldop) 
  if b1=control19 then let oldop=operator: operator=2: refreshpanel(operator,99,oldop) 
  if b1=control20 then let oldop=operator: operator=3: refreshpanel(operator,99,oldop) 
  if b1=control21 then let oldop=operator: operator=4: refreshpanel(operator,99,oldop) 
  if b1=control22 then let oldop=operator: operator=5: refreshpanel(operator,99,oldop) 

' todo: 23,24 selects lfo,settings  
  

   if ctrlstate<2 then lpoke base+2160+96, lpeek(base+2160+96) + $20000000 
  
'  refreshpanel(operator)
  b3=0: b2=0: b1=0: b0=0
  goto 90
endif
  
' ----- MIDI note on 

if b3=$90 andalso b0<>0 then
  let min=$7FFFFFFF: let minc=0
  for i=0 to maxchannel
    if channelassign(i)<min then min=channelassign(i): minc=i  	' find the "oldest" channel
  next i
  channelnotes(minc)=b1                                        	' assign the note to the channel
  base2=base+144*minc
  lpoke base2+100,round(ffreq(0)*notes(b1)*freqv)      		' set a new frequency
  lpoke base2+108,round(ffreq(1)*notes(b1)*freqv)       	' set a new frequency
  lpoke base2+116,round(ffreq(2)*notes(b1)*freqv)       	' set a new frequency
  lpoke base2+124,round(ffreq(3)*notes(b1)*freqv)       	' set a new frequency
'  lpoke base2+100,round(notes(b1)*freqv)         		' set a new frequency
' lpoke base2+104,$4000_0000+(b0*onoffb(1))*64			' set a new volume and trigger the note on ' todo: rest ops are to set!!!
  let vel0=127-senseb(0)+(b0*senseb(0)/127)
  let vel1=127-senseb(1)+(b0*senseb(1)/127)
' let vel2=127-senseb(2)+(b0*senseb(2)/127)
' let vel3=127-senseb(3)+(b0*senseb(3)/127)
   print vel1
   lpoke base2+104, (vel1*onoffb(1))*32			' set a new volume and trigger the note on ' todo: rest ops are to set!!!
'  lpoke base2+112, (vel2*onoffb(2))*32			' set a new volume and trigger the note on ' todo: rest ops are to set!!!
'  lpoke base2+120, (vel3*onoffb(3))*32			' set a new volume and trigger the note on ' todo: rest ops are to set!!!

   lpoke base2+96,$4000_0000+(vel0*onoffb(0))*32				' set a new volume and trigger the note on ' todo: rest ops are to set!!! 128-16*sense+b0*sense)/8
  channelassign(minc)=kbdpressed: kbdpressed+=1 		' update the channel "time" (in key presses)
  b3=0
  kbdnoteon(b1,60,460)
  goto 90
endif

' ----- MIDI note off 

if b3=$80 orelse (b3=$90 andalso b0=0) then
  for i=0 to maxchannel
    if channelnotes(i)=b1 then lpoke base+144*i+96,lpeek(base+144*i+96) + $8000_0000 ' find the channel and trigger the note off
  next i  
  b3=0
  kbdnoteoff(b1,60,460)
  goto 90
endif

if b3=$E0 then 
  let bend!= 2^((256*b0+b1-$4000)/15360.0) ' this gives exact semitones
  for i=0 to 15 : for j=0 to 5: lpoke base+144*i+100+8*j,round(notes(channelnotes(i))*ffreq(j)*freqv*bend!) : next j: next i 
  b3=0
endif  
goto 90

'---------------------------------------------------------------------------------------
'------------------------------- The main loop END -------------------------------------
'---------------------------------------------------------------------------------------

sub loadpreset(presetnum)

dim val1,val2,flashaddr

flashaddr=$880000+256*presetnum
flash.rd_block(flashaddr,256, addr(flashbufb(0)))

for i=0 to 5:   r1b(i)=    flashbufb(i+000) : next i
for i=0 to 5:   r2b(i)=    flashbufb(i+006) : next i
for i=0 to 5:   r3b(i)=    flashbufb(i+012) : next i
for i=0 to 5:   r4b(i)=    flashbufb(i+018) : next i

for i=0 to 5:   l1b(i)=    flashbufb(i+024) : next i
for i=0 to 5:   l2b(i)=    flashbufb(i+030) : next i
for i=0 to 5:   l3b(i)=    flashbufb(i+036) : next i
for i=0 to 5:   l4b(i)=    flashbufb(i+042) : next i

for i=0 to 5:   lvb(i)=    flashbufb(i+048) : next i
for i=0 to 5:   senseb(i)= flashbufb(i+054) : next i
for i=0 to 5:   rtscb(i)=  flashbufb(i+060) : next i
for i=0 to 5:   ldptb(i)=  flashbufb(i+066) : next i

for i=0 to 5:   rdptb(i)=  flashbufb(i+072) : next i
for i=0 to 5:   lcurb(i)=  flashbufb(i+078) : next i
for i=0 to 5:   rcurb(i)=  flashbufb(i+084) : next i
for i=0 to 5:   bkptb(i)=  flashbufb(i+090) : next i

for i=0 to 5:   freqcb(i)= flashbufb(i+096) : next i
for i=0 to 5:   freqfb(i)= flashbufb(i+102) : next i
for i=0 to 5:   freqmb(i)= flashbufb(i+108) : next i
for i=0 to 5:   m0b(i)=    flashbufb(i+114) : next i

for i=0 to 5:   m1b(i)=    flashbufb(i+120) : next i
for i=0 to 5:   m2b(i)=    flashbufb(i+126) : next i
for i=0 to 5:   m3b(i)=    flashbufb(i+132) : next i
for i=0 to 5:   m4b(i)=    flashbufb(i+138) : next i

for i=0 to 5:   m5b(i)=    flashbufb(i+144) : next i
for i=0 to 5:   amsnb(i)=  flashbufb(i+150) : next i
for i=0 to 5:   fmsnb(i)=  flashbufb(i+156) : next i
for i=0 to 5:   onoffb(i)= flashbufb(i+162) : next i


for i=0 to 5
  r1l(i)=round(61*exp(r1b(i)/10.0)) : lpoke 2304+72*i+base+00,r1l(i) : refreshpanel(i,0,r1b(i))	' rate 1  
  r2l(i)=round(61*exp(r2b(i)/10.0)) : lpoke 2304+72*i+base+04,r2l(i) : refreshpanel(i,1,r2b(i))	' rate 2 
  r3l(i)=round(61*exp(r3b(i)/10.0)) : lpoke 2304+72*i+base+08,r3l(i) : refreshpanel(i,2,r3b(i))	' rate 3 
  r4l(i)=round(61*exp(r4b(i)/10.0)) : lpoke 2304+72*i+base+12,r4l(i) : refreshpanel(i,3,r4b(i))	' rate 4  
  
  l1l(i)=$43FF_FFFF-$1C+$78F1E4*l1b(i) :lpoke 2304+72*i+base+16,l1l(i) : refreshpanel(i,4,l1b(i))	' level 1  
  l2l(i)=$43FF_FFFF-$1C+$78F1E4*l2b(i): lpoke 2304+72*i+base+20,l2l(i) : refreshpanel(i,5,l2b(i))	' level 2  
  l3l(i)=$43FF_FFFF-$1C+$78F1E4*l3b(i): lpoke 2304+72*i+base+24,l3l(i) : refreshpanel(i,6,l3b(i))	' level 3  
  l4l(i)=$43FF_FFFF-$1C+$78F1E4*l4b(i): lpoke 2304+72*i+base+28,l4l(i) : refreshpanel(i,7,l4b(i))	' level 4  
  
  m0l(i)=round(exp(m0b(i)/11.4514)) : lpoke 2304+72*i+base+32,m0l(i) : refreshpanel(i,20,m0b(i))	' Modulation level from Operator 0
  m1l(i)=round(exp(m1b(i)/11.4514)) : lpoke 2304+72*i+base+36,m1l(i) : refreshpanel(i,21,m1b(i))	' Modulation level from Operator 1  
  m2l(i)=round(exp(m2b(i)/11.4514)) : lpoke 2304+72*i+base+40,m2l(i) : refreshpanel(i,22,m2b(i))	' Modulation level from Operator 2
  m3l(i)=round(exp(m3b(i)/11.4514)) : lpoke 2304+72*i+base+44,m3l(i) : refreshpanel(i,23,m3b(i))	' Modulation level from Operator 3
  m4l(i)=round(exp(m4b(i)/11.4514)) : lpoke 2304+72*i+base+48,m4l(i) : refreshpanel(i,24,m4b(i))	' Modulation level from Operator 4
  m5l(i)=round(exp(m5b(i)/11.4514)) : lpoke 2304+72*i+base+52,m5l(i) : refreshpanel(i,25,m5b(i))	' Modulation level from Operator 5
    
  lvl(i)=round(exp(lvb(i)/11.4514))/4 : lpoke 2304+72*i+base+56,lvl(i) : refreshpanel(i,8,lvb(i))  	' Operator output level   

  if freqmb(i)=0 then ffreq(i)=((freqcb(i)+1)/4.0)+freqfb(i)/512.0 
  if freqmb(i)=1 then freq1=(freqcb(i)/8)-2: ffreq(i)=2.0^(freq1*1.0+freqfb(i)/128.0)
  refreshpanel(i,16,freqcb(i))
  refreshpanel(i,17,freqfb(i))
  refreshpanel(i,18,freqmb(i))
  refreshpanel(i,19,round(ffreq(i)*1000000))

  amsnl(i)=round(exp(amsnb(i)/11.4514)) : lpoke 2304+72*i+base+60,amsnl(i) : refreshpanel(i,26,amsnb(i))	' LFO AM sensitivity at slider 1
  fmsnl(i)=round(exp(fmsnb(i)/11.4514)) : lpoke 2304+72*i+base+64,fmsnl(i) : refreshpanel(i,27,fmsnb(i))	' LFO FM sensitivity at slider 2
  refreshpanel(i,9,senseb(i))											' KBD velocity sense
  rtscl(i)=round(exp(rtscb(i)/11.4514)) : lpoke 2304+72*i+base+68,rtscl(i) : refreshpanel(i,10,rtscb(i))	' Rate scaling
  refreshpanel(i,11,ldptb(i))											' Left depth
  refreshpanel(i,12,rdptb(i))											' Right depth
  refreshpanel(i,13,lcurb(i))											' Left curve
  refreshpanel(i,14,rcurb(i))											' Right curve
  refreshpanel(i,15,bkptb(i))											' Breakpoint
next i
  
lpoke base+2160+96, lpeek(base+2160+96) + $20000000  ' tell the driver to read these data


end sub


sub savepreset(presetnum)


dim val1,val2,flashaddr

flashaddr=($880000+256*presetnum) and $FFFFF000 
flash.rd_block(flashaddr,4096,addr(flashbufb(0)))

val1=(presetnum mod 16)
for i=0 to 255: flashbufb(256*val1+i)=0             : next i

for i=0 to 5:   flashbufb(256*val1+i+000)=r1b(i)    : next i
for i=0 to 5:   flashbufb(256*val1+i+006)=r2b(i)    : next i
for i=0 to 5:   flashbufb(256*val1+i+012)=r3b(i)    : next i
for i=0 to 5:   flashbufb(256*val1+i+018)=r4b(i)    : next i

for i=0 to 5:   flashbufb(256*val1+i+024)=l1b(i)    : next i
for i=0 to 5:   flashbufb(256*val1+i+030)=l2b(i)    : next i
for i=0 to 5:   flashbufb(256*val1+i+036)=l3b(i)    : next i
for i=0 to 5:   flashbufb(256*val1+i+042)=l4b(i)    : next i

for i=0 to 5:   flashbufb(256*val1+i+048)=lvb(i)    : next i
for i=0 to 5:   flashbufb(256*val1+i+054)=senseb(i) : next i
for i=0 to 5:   flashbufb(256*val1+i+060)=rtscb(i)  : next i
for i=0 to 5:   flashbufb(256*val1+i+066)=ldptb(i)  : next i

for i=0 to 5:   flashbufb(256*val1+i+072)=rdptb(i)  : next i
for i=0 to 5:   flashbufb(256*val1+i+078)=lcurb(i)  : next i
for i=0 to 5:   flashbufb(256*val1+i+084)=rcurb(i)  : next i
for i=0 to 5:   flashbufb(256*val1+i+090)=bkptb(i)  : next i

for i=0 to 5:   flashbufb(256*val1+i+096)=freqcb(i) : next i
for i=0 to 5:   flashbufb(256*val1+i+102)=freqfb(i) : next i
for i=0 to 5:   flashbufb(256*val1+i+108)=freqmb(i) : next i
for i=0 to 5:   flashbufb(256*val1+i+114)=m0b(i)    : next i

for i=0 to 5:   flashbufb(256*val1+i+120)=m1b(i)    : next i
for i=0 to 5:   flashbufb(256*val1+i+126)=m2b(i)    : next i
for i=0 to 5:   flashbufb(256*val1+i+132)=m3b(i)    : next i
for i=0 to 5:   flashbufb(256*val1+i+138)=m4b(i)    : next i

for i=0 to 5:   flashbufb(256*val1+i+144)=m5b(i)    : next i
for i=0 to 5:   flashbufb(256*val1+i+150)=amsnb(i)  : next i
for i=0 to 5:   flashbufb(256*val1+i+156)=fmsnb(i)  : next i
for i=0 to 5:   flashbufb(256*val1+i+162)=onoffb(i) : next i

flash.erase(flashaddr,$20,1)
waitms(20)
for i=0 to 7
  flash.wr_block (flashaddr,256,addr(flashbufb(256*i)))
  waitms(10)
  flashaddr+=256
next i



end sub


'------------------ Prepare the UI

sub preparepanels

' 1. Channel and oscilloscope panel at graphic canvas

cls(15,82)
v.setbordercolor2(88)	
v.setfontfamily(4)						'
v.outtextxycz((1024-16*len(version$))/2,4,version$,92,82,2,2)
v.setfontfamily(0)							'

for i=0 to 7
  v.box(5+256*(i mod 4),40+200*(i/4),252+256*(i mod 4),59+200*(i/4),26+16*i)
  v.box(5+256*(i mod 4),60+200*(i/4),252+256*(i mod 4),228+200*(i/4),19+16*i)
next i  
  
for i=0 to 5  
  v.outtextxycf(  8+256*(i mod 4), 64+200*(i/4), "R1 ",26+16*i) : v.outtextxycf( 40+256*(i mod 4), 64+200*(i/4),v.inttostr2(r1b(i),3),29+16*i)
  v.outtextxycf(  8+256*(i mod 4), 80+200*(i/4), "R2 ",26+16*i) : v.outtextxycf( 40+256*(i mod 4), 80+200*(i/4),v.inttostr2(r2b(i),3),29+16*i)
  v.outtextxycf(  8+256*(i mod 4), 96+200*(i/4), "R3 ",26+16*i) : v.outtextxycf( 40+256*(i mod 4), 96+200*(i/4),v.inttostr2(r3b(i),3),29+16*i)
  v.outtextxycf(  8+256*(i mod 4),112+200*(i/4), "R4 ",26+16*i) : v.outtextxycf( 40+256*(i mod 4),112+200*(i/4),v.inttostr2(r4b(i),3),29+16*i)
  v.outtextxycf(  8+256*(i mod 4),128+200*(i/4), "L1 ",26+16*i) : v.outtextxycf( 40+256*(i mod 4),128+200*(i/4),v.inttostr2(l1b(i),3),29+16*i)
  v.outtextxycf(  8+256*(i mod 4),144+200*(i/4), "L2 ",26+16*i) : v.outtextxycf( 40+256*(i mod 4),144+200*(i/4),v.inttostr2(l2b(i),3),29+16*i)
  v.outtextxycf(  8+256*(i mod 4),160+200*(i/4), "L3 ",26+16*i) : v.outtextxycf( 40+256*(i mod 4),160+200*(i/4),v.inttostr2(l3b(i),3),29+16*i)
  v.outtextxycf(  8+256*(i mod 4),176+200*(i/4), "L4 ",26+16*i) : v.outtextxycf( 40+256*(i mod 4),176+200*(i/4),v.inttostr2(l4b(i),3),29+16*i)
  v.outtextxycf(  8+256*(i mod 4),192+200*(i/4), "Lv ",26+16*i) : v.outtextxycf( 40+256*(i mod 4),192+200*(i/4),v.inttostr2(lvb(i),3),29+16*i)
  v.outtextxycf(  8+256*(i mod 4),208+200*(i/4), "Sen",26+16*i) : v.outtextxycf( 40+256*(i mod 4),208+200*(i/4),v.inttostr2(senseb(i),3),29+16*i)

  v.outtextxycf( 80+256*(i mod 4), 64+200*(i/4),"RtSc",26+16*i) : v.outtextxycf(120+256*(i mod 4), 64+200*(i/4),v.inttostr2(rtscb(i),3),29+16*i)
  v.outtextxycf( 80+256*(i mod 4), 80+200*(i/4),"Ldpt",26+16*i) : v.outtextxycf(120+256*(i mod 4), 80+200*(i/4),v.inttostr2(ldptb(i),3),29+16*i)
  v.outtextxycf( 80+256*(i mod 4), 96+200*(i/4),"Rdpt",26+16*i) : v.outtextxycf(120+256*(i mod 4), 96+200*(i/4),v.inttostr2(rdptb(i),3),29+16*i)
  v.outtextxycf( 80+256*(i mod 4),112+200*(i/4),"LCur",26+16*i) : v.outtextxycf(120+256*(i mod 4),112+200*(i/4),"+Lin",29+16*i)
  v.outtextxycf( 80+256*(i mod 4),128+200*(i/4),"RCur",26+16*i) : v.outtextxycf(120+256*(i mod 4),128+200*(i/4),"+Lin",29+16*i)
  v.outtextxycf( 80+256*(i mod 4),144+200*(i/4),"Bkpt",26+16*i) : v.outtextxycf(120+256*(i mod 4),144+200*(i/4),"C4",29+16*i)
  v.outtextxycf( 80+256*(i mod 4),160+200*(i/4),"FrqC",26+16*i) : v.outtextxycf(120+256*(i mod 4),160+200*(i/4),v.inttostr2(freqcb(i),3),29+16*i)
  v.outtextxycf( 80+256*(i mod 4),176+200*(i/4),"FrqF",26+16*i) : v.outtextxycf(120+256*(i mod 4),176+200*(i/4),v.inttostr2(freqfb(i),3),29+16*i)
  v.outtextxycf( 80+256*(i mod 4),192+200*(i/4),"FrqM",26+16*i) : v.outtextxycf(120+256*(i mod 4),192+200*(i/4),"Rate", 29+16*i)
  v.outtextxycf( 80+256*(i mod 4),208+200*(i/4),"Freq",26+16*i) : v.outtextxycf(120+256*(i mod 4),208+200*(i/4),"1.000000",29+16*i)

  v.outtextxycf(168+256*(i mod 4), 64+200*(i/4),"Mod1",26+16*i) : v.outtextxycf(208+256*(i mod 4),64+200*(i/4),v.inttostr2(m0b(i),3),29+16*i)
  v.outtextxycf(168+256*(i mod 4), 80+200*(i/4),"Mod2",26+16*i) : v.outtextxycf(208+256*(i mod 4),80+200*(i/4),v.inttostr2(m1b(i),3),29+16*i)
  v.outtextxycf(168+256*(i mod 4), 96+200*(i/4),"Mod3",26+16*i) : v.outtextxycf(208+256*(i mod 4),96+200*(i/4),v.inttostr2(m2b(i),3),29+16*i)
  v.outtextxycf(168+256*(i mod 4),112+200*(i/4),"Mod4",26+16*i) : v.outtextxycf(208+256*(i mod 4),112+200*(i/4),v.inttostr2(m3b(i),3),29+16*i)
  v.outtextxycf(168+256*(i mod 4),128+200*(i/4),"Mod5",26+16*i) : v.outtextxycf(208+256*(i mod 4),128+200*(i/4),v.inttostr2(m4b(i),3),29+16*i)
  v.outtextxycf(168+256*(i mod 4),144+200*(i/4),"Mod6",26+16*i) : v.outtextxycf(208+256*(i mod 4),144+200*(i/4),v.inttostr2(m5b(i),3),29+16*i)
  v.outtextxycf(168+256*(i mod 4),160+200*(i/4),"AMSn",26+16*i) : v.outtextxycf(208+256*(i mod 4),160+200*(i/4),v.inttostr2(amsnb(i),3),29+16*i)
  v.outtextxycf(168+256*(i mod 4),176+200*(i/4),"FMSn",26+16*i) : v.outtextxycf(208+256*(i mod 4),176+200*(i/4),v.inttostr2(fmsnb(i),3),29+16*i)

  v.outtextxycf(12+256*(i mod 4),43+200*(i/4),"Operator "+v.inttostr2(i,1),15)
next i
v.frame(4,39,253,227,15)

v.outtextxycf(12+256*(2 mod 4),43+200,"LFO+Frequency Envelope",15)
v.outtextxycf(12+256*(3 mod 4),43+200,"Settings ",15)

'v.outtextxycf(8+200,43,"Operator 2 ",15)


end sub

sub refreshpanel(i,ctrl,val1)

dim value$

if ctrl<10 then v.outtextxycg( 40+256*(i mod 4), 64+ctrl*16+200*(i/4),v.inttostr2(val1,3),29+16*i,19+16*i)
if ctrl>=10 andalso ctrl<20 then
  value$=decuns$(val1,3)
  if ctrl=19 then value$=left$(decuns$(val1/1000000)+"."+decuns$(val1 mod 1000000,6),8)
  if ctrl=13 orelse ctrl=14 then
    if (val1 mod 4)=0 then value$="+Lin"
    if (val1 mod 4)=1 then value$="-Lin"
    if (val1 mod 4)=2 then value$="+Exp"
    if (val1 mod 4)=3 then value$="-Exp"
  endif  
  if ctrl=15 then value$=left$(notes$(val1 mod 12)+str$((val1/12)-1)+"    ",4)
  if ctrl=18 then 
    if (val1 mod 2)=0 then value$="Rate" else value$="Hz  "
  endif
  v.outtextxycg(120+256*(i mod 4), 64+(ctrl-10)*16+200*(i/4),value$,29+16*i,19+16*i)
endif
if ctrl>=20 then v.outtextxycg(208+256*(i mod 4), 64+(ctrl-20)*16+200*(i/4),v.inttostr2(val1,3),29+16*i,19+16*i)

if ctrl=99 then v.frame(4+256*(val1 mod 4),39+200*(val1/4),253+256*(val1 mod 4),227+200*(val1/4),144) : v.frame(4+256*(i mod 4),39+200*(i/4),253+256*(i mod 4),227+200*(i/4),15) 
if ctrl=98 then v.box(5+200*(i mod 4),41,196+200*(i mod 4),59,17+9*val1+16*i) : v.outtextxycf(200*(i mod 4)+8,43,"Operator "+decuns$(i+1),15)
end sub

sub kbddraw(x,y)

for o=0 to 10
  for n=0 to 6
    if o=10 andalso n>4 then continue
    v.frame(x+0+84*o+12*n,  y,x+12+84*o+12*n,y+90,0 )        'white
    v.box  (x+1+84*o+12*n+1,y,x+11+84*o+12*n,y+89,15)
  next n
               v.box(x+08+12*(7*o),y,x+16+12*(7*o),y+60,0)  'black
               v.box(x+20+12*(7*o),y,x+28+12*(7*o),y+60,0)
               v.box(x+44+12*(7*o),y,x+52+12*(7*o),y+60,0)
  if o<10 then v.box(x+56+12*(7*o),y,x+64+12*(7*o),y+60,0)
  if o<10 then v.box(x+68+12*(7*o),y,x+76+12*(7*o),y+60,0)
  if o>=1 then v.outtextxycf(x+3+84*o,y+74,v.inttostr(o-1),0)
next o 

end sub


sub kbdnoteoff(note,x,y)

dim n,o,xx,yy
o=b1/12: n=b1 mod 12: xx=x+84*o
select case n
  case 0
    v.box(xx+3,y+65,xx+10,y+73,15)
  case 1
    v.box(xx+10,y+35,xx+14,y+45,0)
  case 2
    v.box(xx+12+2,y+65,xx+12+10,y+73,15)
  case 3
    v.box(xx+12+10,y+35,xx+12+14,y+45,0)
  case 4
    v.box(xx+24+2,y+65,xx+24+10,y+73,15)
  case 5
    v.box(xx+36+2,y+65,xx+36+10,y+73,15)
  case 6
    v.box(xx+36+10,y+35,xx+36+14,y+45,0)
  case 7
    v.box(xx+48+2,y+65,xx+48+10,y+73,15)
  case 8
    v.box(xx+48+10,y+35,xx+48+14,y+45,0)
  case 9
    v.box(xx+60+2,y+65,xx+60+10,y+73,15)
  case 10
    v.box(xx+60+10,y+35,xx+60+14,y+45,0)
  case 11
    v.box(xx+72+2,y+65,xx+72+10,y+73,15)  
  end select
end sub


sub kbdnoteon(note,x,y)

dim n,o,xx,yy
o=b1/12: n=b1 mod 12: xx=x+84*o
select case n
  case 0
    v.box(xx+3,y+65,xx+10,y+73,248)
  case 1
    v.box(xx+10,y+35,xx+14,y+45,248)
  case 2
    v.box(xx+12+3,y+65,xx+12+10,y+73,248)
  case 3
    v.box(xx+12+10,y+35,xx+12+14,y+45,248)
  case 4
    v.box(xx+24+3,y+65,xx+24+10,y+73,248)
  case 5
    v.box(xx+36+3,y+65,xx+36+10,y+73,248)
  case 6
    v.box(xx+36+10,y+35,xx+36+14,y+45,248)
  case 7
    v.box(xx+48+3,y+65,xx+48+10,y+73,248)
  case 8
    v.box(xx+48+10,y+35,xx+48+14,y+45,248)
  case 9
    v.box(xx+60+3,y+65,xx+60+10,y+73,248)
  case 10
    v.box(xx+60+10,y+35,xx+60+14,y+45,248)
  case 11
    v.box(xx+72+3,y+65,xx+72+10,y+73,248)  
  end select
end sub
