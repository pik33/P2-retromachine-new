'-----------------------------------------------------------------------
' Frequency modulation sound toy
' v. 0.04 - 20220711
' pik33@o2.pl
'-----------------------------------------------------------------------

'sample rate=82264.77587890625
const HEAPSIZE = 16384
#include "retromachine.bi"


const version$="Propeller 2 FM synth v.0.07"


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

dim notes(127) as single 
dim shared notes$(11)={"C","C#","D","D#","E","F","F#","G","G#","A","A#","H"}
dim oct(7)
dim r1,r2,r3,r4,l1,l2,l3,l4,lv,sense,freqc,freqf,m0,m1,m2,m3,m4,m5,m6,freqmode,onoff as ulong(7)  ' these are 20 bytes per op! 
dim ffreq as single(7)
dim slider1,slider2,slider3,slider4,slider5,slider6,slider7,slider8,slider9 as ulong
dim knob1,knob2,knob3,knob4,knob5,knob6,knob7,knob8,knob9 as ulong
dim switch1,switch2,switch3,switch4,switch5,switch6,switch7,switch8,switch9 as ulong
dim operator, ctrlstate, kbdpressed, base2, change as ulong
dim channelassign(31)
dim channelnotes(31)
dim midi as ulong
declare midibytes alias midi as ubyte(3)
dim flashbuf as ulong(1023)




' ----------- start cogs'
flash.start(2000) ' presets at $880000 Preset is 96 longs, add reserved to 128 longs=512 bytes=8 presets per page
startpsram()
startmidi()
startvideo()
startaudio()

'mount "/sd", _vfs_open_sdcard()
'chdir "/sd/FM/"
'let currentdir$="/sd/FM/"
' ----------- init variables

ctrlstate=0 ' ADSR via sliders
 
switch1=0: switch2=0 : switch3=0: switch4=0: switch5=0: switch6=0: switch7=0: switch8=0 :switch9=0  
knob1=0: knob2=0 : knob3=0 : knob4=0: knob5=0: knob6=0: knob7=0: knob8=0 :knob9=0  
slider1=127: slider2=127 : slider3=127: slider4=127: slider5=127: slider6=127: slider7=127: slider8=0 :slider9=127 :operator=0

for i=0 to 7 

  r1(i)=round(61*exp(slider1/10.0)) 
  r2(i)=round(61*exp(slider2/10.0))
  r3(i)=round(61*exp(slider3/10.0))
  r4(i)=round(61*exp(slider4/10.0)) 

  l1(i)=$43FF_FFFF-$1C+$78F1E4*slider5  
  l2(i)=$43FF_FFFF-$1C+$78F1E4*slider6  
  l3(i)=$43FF_FFFF-$1C+$78F1E4*slider7  
  l4(i)=$43FF_FFFF-$1C+$78F1E4*slider8
'  if i=0 then 
  lv(i)=$FFFF 
'  else lv(i)=0
 'feedback(i)=0 
 m0(i)=0: m1(i)=0: m2(i)=0 : m3(i)=0 : m4(i)=0 :m5(i)=0 :m6(i)=0
 onoff(i)=1
 freqmode(i)=0 : freqc(i)=3 : freqf(i)=0 : ffreq(i)=((freqc(i)+1)/4.0)+freqf(i)/512.0
 if i=0 then sense(i)=8 else sense(i)=0
next i
operator=0: 'change=1
m6(0)=round(exp(127.0/11.4514))/4 
var f#=c_1: for i=0 to 127: notes(i)=f# : f#=f#*c212: next i

for i=0 to maxchannel: channelassign(i)=0 : next i
kbdpressed=1
change=1

' ------------ init channel data
 
for i=0 to 15

lpoke 144*i+base+00,0
lpoke 144*i+base+04,0
lpoke 144*i+base+08,%%1111__0_1111111_0_1_111111_0_11 
lpoke 144*i+base+12,0 

lpoke 144*i+base+16,0
lpoke 144*i+base+20,0
lpoke 144*i+base+24,%0111111111111
lpoke 144*i+base+28,0 

lpoke 144*i+base+32,0
lpoke 144*i+base+36,0
lpoke 144*i+base+40,%0111111111111
lpoke 144*i+base+44,0 

lpoke 144*i+base+48,0
lpoke 144*i+base+52,0
lpoke 144*i+base+56,%0111111111111
lpoke 144*i+base+60,0 

lpoke 144*i+base+64,0
lpoke 144*i+base+68,0
lpoke 144*i+base+72,%0111111111111
lpoke 144*i+base+76,0 

lpoke 144*i+base+80,0
lpoke 144*i+base+84,0
lpoke 144*i+base+88,%0111111111111
lpoke 144*i+base+92,0 

lpoke 144*i+base+96,0
lpoke 144*i+base+100,0
lpoke 144*i+base+104,0
lpoke 144*i+base+108,0
lpoke 144*i+base+112,0
lpoke 144*i+base+116,0
lpoke 144*i+base+120,0
lpoke 144*i+base+124,0
lpoke 144*i+base+128,0
lpoke 144*i+base+132,0
lpoke 144*i+base+136,0
lpoke 144*i+base+140,0
next i

for i=0 to 5
lpoke 2304+72*i+base+00,r1(i)
lpoke 2304+72*i+base+04,$1200000
lpoke 2304+72*i+base+08,$1200000
lpoke 2304+72*i+base+12,$1200000 ' 0.7 ms
lpoke 2304+72*i+base+16,$7FFF_FFFF
lpoke 2304+72*i+base+20,$7FFF_0000
lpoke 2304+72*i+base+24,$7FFF_0000
lpoke 2304+72*i+base+28,$3FFF_FFFF
lpoke 2304+72*i+base+32,m0(i)
lpoke 2304+72*i+base+36,m1(i)
lpoke 2304+72*i+base+40,m2(i)
lpoke 2304+72*i+base+44,m3(i)
lpoke 2304+72*i+base+48,m4(i)
lpoke 2304+72*i+base+52,m5(i)
lpoke 2304+72*i+base+56,m6(i)
lpoke 2304+72*i+base+60,0
next i
 lpoke base+2160+96, lpeek(base+2160+96) + $20000000  
'------- prepare the UI




preparepanels
kbddraw(60,460)
waitms(1000)
'---------------------------------------------------------------------------------------
'------------------------------- THE MAIN LOOP -----------------------------------------
'---------------------------------------------------------------------------------------

90 
if change>0 then
  if ctrlstate=0 then 

    lv(operator)=$FFFF
 
    lpoke 2304+72*(change-1)+base+00,r1(change-1)
    lpoke 2304+72*(change-1)+base+04,r2(change-1)
    lpoke 2304+72*(change-1)+base+08,r3(change-1)
    lpoke 2304+72*(change-1)+base+12,r4(change-1)
    lpoke 2304+72*(change-1)+base+16,l1(change-1)
    lpoke 2304+72*(change-1)+base+20,l2(change-1)
    lpoke 2304+72*(change-1)+base+24,l3(change-1)
    lpoke 2304+72*(change-1)+base+28,l4(change-1)
    lpoke 2304+72*(change-1)+base+32,m0(change-1)
    lpoke 2304+72*(change-1)+base+36,m1(change-1)
    lpoke 2304+72*(change-1)+base+40,m2(change-1)
    lpoke 2304+72*(change-1)+base+44,m3(change-1)
    lpoke 2304+72*(change-1)+base+48,m4(change-1)
    lpoke 2304+72*(change-1)+base+52,m5(change-1)
    lpoke 2304+72*(change-1)+base+56,m6(change-1)
 '   lpoke 2304+72*operator+base+36,$0
  
    lpoke base+2160+96, lpeek(base+2160+96) + $20000000  ' tell the audio driver to read new data
  endif
  change=0
endif 

midi=rm.readmidi()
if midi<>0 then position 2,35 : v.write ("Midi command: "): v.write(v.inttohex(midi,8)) 
position 2*30,35: v.write("Channel time: "): v.write(v.inttostr2(lpeek($70),3)): v.write(" "): v.write(v.inttohex(lpeek(base+4),8)): v.write(" "):v.write(v.inttohex(lpeek(base+8),8))
if midi=0 then goto 90
let b3=midibytes(3): let b0=midibytes(0): let b1=midibytes(1) : let b2=midibytes(2)

'----- MIDI control change

if b3=$B0 then 

  if b1=control00 then slider1=b0 : r1(operator)=round(61*exp(slider1/10.0)) : 	  change=operator+1 : refreshpanel(operator,0,b0)	' rate 1 at slider 1
  if b1=control01 then slider2=b0 : r2(operator)=round(61*exp(slider2/10.0)) :    change=operator+1 : refreshpanel(operator,1,b0)	' rate 2 at slider 2
  if b1=control02 then slider3=b0 : r3(operator)=round(61*exp(slider3/10.0)) :    change=operator+1 : refreshpanel(operator,2,b0)	' rate 3 at slider 3
  if b1=control03 then slider4=b0 : r4(operator)=round(61*exp(slider4/10.0)) :    change=operator+1 : refreshpanel(operator,3,b0)	' rate 4 st slider 4
  if b1=control04 then slider5=b0 : l1(operator)=$43FF_FFFF-$1C+$78F1E4*slider5 : change=operator+1 : refreshpanel(operator,4,b0)	' level 1 at slider 5
  if b1=control05 then slider6=b0 : l2(operator)=$43FF_FFFF-$1C+$78F1E4*slider6 : change=operator+1 : refreshpanel(operator,5,b0)	' level 2 at slider 6
  if b1=control06 then slider7=b0 : l3(operator)=$43FF_FFFF-$1C+$78F1E4*slider7 : change=operator+1 : refreshpanel(operator,6,b0)	' level 3 at slider 7
  if b1=control07 then slider8=b0 : l4(operator)=$43FF_FFFF-$1C+$78F1E4*slider8 : change=operator+1 : refreshpanel(operator,7,b0)	' level 4 at slider 8
  if b1=control08 then slider9=b0 : m6(operator)=round(exp(slider9/11.4514))/4  : change=operator+1 : refreshpanel(operator,8,b0)' Overall operator level, change=0 as it will be serviced at the main cog 
  if b1=control09 then knob1=b0 :   m0(operator)=round(exp(knob1/11.4514)) :	  change=operator+1 : refreshpanel(operator,12,b0)	' Feedback level
  if b1=control10 then knob2=b0 :   m1(operator)=round(exp(knob2/11.4514)) :      change=operator+1 : refreshpanel(operator,13,b0)	' Feedback level
  if b1=control11 then knob3=b0 :   m2(operator)=round(exp(knob3/11.4514)) :      change=operator+1 : refreshpanel(operator,14,b0)	' Feedback level
  if b1=control12 then knob4=b0 :   m3(operator)=round(exp(knob4/11.4514)) :      change=operator+1 : refreshpanel(operator,15,b0)	' Feedback level
  if b1=control13 then knob5=b0 :   m4(operator)=round(exp(knob5/11.4514)) :      change=operator+1 : refreshpanel(operator,16,b0)	' Feedback level
  if b1=control14 then knob6=b0 :   m5(operator)=round(exp(knob6/11.4514)) :      change=operator+1 : refreshpanel(operator,17,b0)	' Feedback level


 '' freq and kbd sense has no channel values


  if b1=control15 then knob7=b0 :  freqc(operator)=b0 : refreshpanel(operator,10,b0)	:  for i=0 to 15 :  lpoke base+144*i+100+8*operator,round(notes(channelnotes(i))*ffreq(operator)*freqv) : next i         ' Frequency coarse level, serviced at the main cog
  if b1=control16 then knob8=b0 :  freqf(operator)=b0 : refreshpanel(operator,11,b0)	:  for i=0 to 15 :  lpoke base+144*i+100+8*operator,round(notes(channelnotes(i))*ffreq(operator)*freqv) : next i       ' Frequency fine level, serviced at the main cog
  
  
  
  if b1=control25 then 
    sense(operator)=b0 : change=1
    refreshpanel(operator,9,b0) 
  endif  
  
  
  
  
  if b1=control23 then 
    let oldop=operator: operator=operator+1 : if operator=2 then operator=0
    refreshpanel(operator,99,oldop) 
  endif  
  if b1=control17 then 
    onoff(0)=b0/127
    refreshpanel(0,98,onoff(0)) 
  endif  
  if b1=control18 then 
    onoff(1)=b0/127
    refreshpanel(1,98,onoff(1)) 
  endif  
  if b1=control24 then ' button 8 - presets
    v.outtextxycg(8,340,"Select the preset with a key                      ",15,82)
    do : midi=rm.readmidi() : loop until midibytes(3)=$90
    v.outtextxycg(8,340,"Preset "+v.inttostr2(midibytes(1),3)+" --- C2 load, C7 save, other exit",15,82) ' TODO name the preset
    let presetnum=midibytes(1)
    do : midi=rm.readmidi() : loop until midibytes(3)=$90    
    if midibytes(1)=$24 then ' C2, load a preset
       v.outtextxycg(8,340,"Press C4 to load a preset "+v.inttostr2(presetnum,3)+", any other key to cancel ",15,82)
      do : midi=rm.readmidi() : loop until midibytes(3)=$90
      if midibytes(1)=$3C then
        loadpreset(presetnum)
        v.box(8,340,799,355,82): v.outtextxycg(8,340,"Preset "+v.inttostr2(presetnum,3)+" loaded ",15,82)
      else
        v.box(8,340,799,355,82)
      endif   
    endif
    if midibytes(1)=$60 then ' C7, save a preset
       v.outtextxycg(8,340,"Press C4 to save a preset "+v.inttostr2(presetnum,3)+", any other key to cancel ",15,82)
       do : midi=rm.readmidi() : loop until midibytes(3)=$90
      if midibytes(1)=$3C then
        savepreset(presetnum)
        v.box(8,340,799,355,82): v.outtextxycg(8,340,"Preset "+v.inttostr2(presetnum,3)+" saved ",15,82)
      else
        v.box(8,340,799,355,82)
      endif   

       
    endif
    
  endif
  
  
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
  lpoke base2+100,round(ffreq(0)*notes(b1)*freqv)         		' set a new frequency
  lpoke base2+108,round(ffreq(1)*notes(b1)*freqv)         		' set a new frequency
'  lpoke base2+100,round(notes(b1)*freqv)         		' set a new frequency
  lpoke base2+104,$4000_0000+(b0*onoff(1))*64			' set a new volume and trigger the note on ' todo: rest ops are to set!!!
  let vel0=128-16*sense(0)+(b0*sense(0)/8): if vel0=128 then vel0=127
  let vel1=128-16*sense(1)+(b0*sense(1)/8): if vel1=128 then vel1=127
   lpoke base2+104,$4000_0000+(vel1*onoff(1))*64			' set a new volume and trigger the note on ' todo: rest ops are to set!!!

   lpoke base2+96,$4000_0000+(vel0*onoff(0))*64				' set a new volume and trigger the note on ' todo: rest ops are to set!!! 128-16*sense+b0*sense)/8
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

flashaddr=$880000+512*presetnum
flash.rd_block(flashaddr,256, base+2304)
waitms(10)
flash.rd_block(flashaddr+256,256, base+2304+256)

'now reverse engineer this to display

for o=0 to 1 '(6)

  val1=round(10*log(lpeek(base+2304+64*o+00)/61.0)): v.outtextxycg(32+200*o,64 ,v.inttostr2(val1,3),29+16*o,19+16*o)       
  val1=round(10*log(lpeek(base+2304+64*o+04)/61.0)): v.outtextxycg(32+200*o,80 ,v.inttostr2(val1,3),29+16*o,19+16*o)    
  val1=round(10*log(lpeek(base+2304+64*o+08)/61.0)): v.outtextxycg(32+200*o,96, v.inttostr2(val1,3),29+16*o,19+16*o)       
  val1=round(10*log(lpeek(base+2304+64*o+12)/61.0)): v.outtextxycg(32+200*o,112,v.inttostr2(val1,3),29+16*o,19+16*o)     

  val1=(lpeek(base+2304+64*o+16)-$43FF_FFFF+$1C)/$78F1E4 : v.outtextxycg(32+200*o,128,v.inttostr2(val1,3),29+16*o,19+16*o)                                
  val1=(lpeek(base+2304+64*o+20)-$43FF_FFFF+$1C)/$78F1E4 : v.outtextxycg(32+200*o,144,v.inttostr2(val1,3),29+16*o,19+16*o)                             
  val1=(lpeek(base+2304+64*o+24)-$43FF_FFFF+$1C)/$78F1E4 : v.outtextxycg(32+200*o,160,v.inttostr2(val1,3),29+16*o,19+16*o)                                 
  val1=(lpeek(base+2304+64*o+28)-$43FF_FFFF+$1C)/$78F1E4 : v.outtextxycg(32+200*o,176,v.inttostr2(val1,3),29+16*o,19+16*o)                                 

  val1=round(11.4514*log(lpeek(base+2304+64*o+32))) : v.outtextxycg(168+200*o,64,v.inttostr2(val1,3),29+16*o,19+16*o)
  val1=round(11.4514*log(lpeek(base+2304+64*o+36))) : v.outtextxycg(168+200*o,80,v.inttostr2(val1,3),29+16*o,19+16*o)
  val1=round(11.4514*log(lpeek(base+2304+64*o+40))) : v.outtextxycg(168+200*o,96,v.inttostr2(val1,3),29+16*o,19+16*o)
  val1=round(11.4514*log(lpeek(base+2304+64*o+44))) : v.outtextxycg(168+200*o,112,v.inttostr2(val1,3),29+16*o,19+16*o)
  val1=round(11.4514*log(lpeek(base+2304+64*o+48))) : v.outtextxycg(168+200*o,128,v.inttostr2(val1,3),29+16*o,19+16*o)
  val1=round(11.4514*log(lpeek(base+2304+64*o+52))) : v.outtextxycg(168+200*o,144,v.inttostr2(val1,3),29+16*o,19+16*o)
    
  val1=peek(base+2304+6*64+4*o): v.outtextxycg(104+200*o,96,v.inttostr2(val1,3),29+16*o,19+16*o)  'freqc
  val2=peek(base+2304+6*64+4*o+1): v.outtextxycg(104+200*o,112,v.inttostr2(val2,3),29+16*o,19+16*o) 'freqv
  ffreq(o)=((val1+1)/4.0)+val2/512.0: let ffreq$=left$(str$(ffreq(o))+"        ",8) : v.outtextxycg(112+200*o,176,ffreq$,29+16*o,19+16*o) '
  val1=peek(base+2304+6*64+4*o+3): v.outtextxycg(104+200*o,80,v.inttostr2(val1,3),29+16*o,19+16*o)  ' kbd volume sense
   lpoke base+2160+96, lpeek(base+2160+96) + $20000000 
next o

end sub


sub savepreset(presetnum)

dim val1,val2,flashaddr

position 0,0

flashaddr=($880000+512*presetnum) and $FFFFF000 : print hex$(flashaddr) 
flash.rd_block(flashaddr,4096,addr(flashbuf))
val1=(presetnum mod 8)
for i=0 to 127: flashbuf(128*val1+i)=0: next i
for i=0 to 95: flashbuf(128*val1+i)=lpeek(base+2304+4*i): next i
for i=0 to 5 : val2=freqc(i)+256*freqf(i)+65536*sense(i) : flashbuf(128*val1+96+i)=val2 : next i
flash.erase(flashaddr,$20,1)
waitms(20)
for i=0 to 7
  flash.wr_block (flashaddr,256,addr(flashbuf(64*i)))
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
  v.outtextxycf(  8+256*(i mod 4), 64+200*(i/4), "R1 ",26+16*i) : v.outtextxycf( 40+256*(i mod 4), 64+200*(i/4),v.inttostr2(slider1,3),29+16*i)
  v.outtextxycf(  8+256*(i mod 4), 80+200*(i/4), "R2 ",26+16*i) : v.outtextxycf( 40+256*(i mod 4), 80+200*(i/4),v.inttostr2(slider2,3),29+16*i)
  v.outtextxycf(  8+256*(i mod 4), 96+200*(i/4), "R3 ",26+16*i) : v.outtextxycf( 40+256*(i mod 4), 96+200*(i/4),v.inttostr2(slider3,3),29+16*i)
  v.outtextxycf(  8+256*(i mod 4),112+200*(i/4), "R4 ",26+16*i) : v.outtextxycf( 40+256*(i mod 4),112+200*(i/4),v.inttostr2(slider4,3),29+16*i)
  v.outtextxycf(  8+256*(i mod 4),128+200*(i/4), "L1 ",26+16*i) : v.outtextxycf( 40+256*(i mod 4),128+200*(i/4),v.inttostr2(slider5,3),29+16*i)
  v.outtextxycf(  8+256*(i mod 4),144+200*(i/4), "L2 ",26+16*i) : v.outtextxycf( 40+256*(i mod 4),144+200*(i/4),v.inttostr2(slider6,3),29+16*i)
  v.outtextxycf(  8+256*(i mod 4),160+200*(i/4), "L3 ",26+16*i) : v.outtextxycf( 40+256*(i mod 4),160+200*(i/4),v.inttostr2(slider7,3),29+16*i)
  v.outtextxycf(  8+256*(i mod 4),176+200*(i/4), "L4 ",26+16*i) : v.outtextxycf( 40+256*(i mod 4),176+200*(i/4),v.inttostr2(slider8,3),29+16*i)
  v.outtextxycf(  8+256*(i mod 4),192+200*(i/4), "Lv ",26+16*i) : v.outtextxycf( 40+256*(i mod 4),192+200*(i/4),v.inttostr2(m6(i)/128,3),29+16*i)
  v.outtextxycf(  8+256*(i mod 4),208+200*(i/4), "Sen",26+16*i) : v.outtextxycf( 40+256*(i mod 4),208+200*(i/4),v.inttostr2(switch9,3),29+16*i)

  v.outtextxycf( 80+256*(i mod 4), 64+200*(i/4),"RtSc",26+16*i) : v.outtextxycf(120+256*(i mod 4), 64+200*(i/4),v.inttostr2(m0(i),3),29+16*i)
  v.outtextxycf( 80+256*(i mod 4), 80+200*(i/4),"Ldpt",26+16*i) : v.outtextxycf(120+256*(i mod 4), 80+200*(i/4),v.inttostr2(m0(i),3),29+16*i)
  v.outtextxycf( 80+256*(i mod 4), 96+200*(i/4),"Rdpt",26+16*i) : v.outtextxycf(120+256*(i mod 4), 96+200*(i/4),v.inttostr2(m0(i),3),29+16*i)
  v.outtextxycf( 80+256*(i mod 4),112+200*(i/4),"LCur",26+16*i) : v.outtextxycf(120+256*(i mod 4),112+200*(i/4),"+Lin",29+16*i)
  v.outtextxycf( 80+256*(i mod 4),128+200*(i/4),"RCur",26+16*i) : v.outtextxycf(120+256*(i mod 4),128+200*(i/4),"+Lin",29+16*i)
  v.outtextxycf( 80+256*(i mod 4),144+200*(i/4),"Bkpt",26+16*i) : v.outtextxycf(120+256*(i mod 4),144+200*(i/4),"C4",29+16*i)
  v.outtextxycf( 80+256*(i mod 4),160+200*(i/4),"FrqC",26+16*i) : v.outtextxycf(120+256*(i mod 4),160+200*(i/4),v.inttostr2(knob7,3),29+16*i)
  v.outtextxycf( 80+256*(i mod 4),176+200*(i/4),"FrqF",26+16*i) : v.outtextxycf(120+256*(i mod 4),176+200*(i/4),v.inttostr2(knob8,3),29+16*i)
  v.outtextxycf( 80+256*(i mod 4),192+200*(i/4),"FrqM",26+16*i) : v.outtextxycf(120+256*(i mod 4),192+200*(i/4),"Rate", 29+16*i)
  v.outtextxycf( 80+256*(i mod 4),208+200*(i/4),"Freq",26+16*i) : v.outtextxycf(120+256*(i mod 4),208+200*(i/4),"1",29+16*i)

  v.outtextxycf(168+256*(i mod 4), 64+200*(i/4),"Mod1",26+16*i) : v.outtextxycf(208+256*(i mod 4),64+200*(i/4),v.inttostr2(m0(i),3),29+16*i)
  v.outtextxycf(168+256*(i mod 4), 80+200*(i/4),"Mod2",26+16*i) : v.outtextxycf(208+256*(i mod 4),80+200*(i/4),v.inttostr2(m1(i),3),29+16*i)
  v.outtextxycf(168+256*(i mod 4), 96+200*(i/4),"Mod3",26+16*i) : v.outtextxycf(208+256*(i mod 4),96+200*(i/4),v.inttostr2(m2(i),3),29+16*i)
  v.outtextxycf(168+256*(i mod 4),112+200*(i/4),"Mod4",26+16*i) : v.outtextxycf(208+256*(i mod 4),112+200*(i/4),v.inttostr2(m3(i),3),29+16*i)
  v.outtextxycf(168+256*(i mod 4),128+200*(i/4),"Mod5",26+16*i) : v.outtextxycf(208+256*(i mod 4),128+200*(i/4),v.inttostr2(m4(i),3),29+16*i)
  v.outtextxycf(168+256*(i mod 4),144+200*(i/4),"Mod6",26+16*i) : v.outtextxycf(208+256*(i mod 4),144+200*(i/4),v.inttostr2(m5(i),3),29+16*i)
  v.outtextxycf(168+256*(i mod 4),160+200*(i/4),"AMSn",26+16*i) : v.outtextxycf(208+256*(i mod 4),160+200*(i/4),v.inttostr2(m5(i),3),29+16*i)

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
  if ctrl=13 orelse ctrl=14 then
    if (val1 mod 4)=0 then value$="+Lin"
    if (val1 mod 4)=1 then value$="-Lin"
    if (val1 mod 4)=2 then value$="+Exp"
    if (val1 mod 4)=3 then value$="-Exp"
  endif  
  if ctrl=15 then value$=notes$(val1 mod 12)+str$((val1/12)-1)
  if ctrl=18 then 
    if (val1 mod 2)=0 then value$="Rate" else value$="Hz"
  endif
  v.outtextxycg(120+256*(i mod 4), 64+(ctrl-10)*16+200*(i/4),value$,29+16*i,19+16*i)
endif
if ctrl>=20 then v.outtextxycg(208+256*(i mod 4), 64+(ctrl-20)*16+200*(i/4),v.inttostr2(val1,3),29+16*i,19+16*i)

if ctrl=99 then v.frame(4+256*(val1 mod 4),39+200*(val1/4),253+256*(val1 mod 4),227+200*(val1/4),144) : v.frame(4+256*(i mod 4),39+200*(i/4),253+256*(i mod 4),227+200*(i/4),15) 
if ctrl=98 then v.box(5+200*(i mod 4),41,196+200*(i mod 4),59,17+9*val1+16*i) : v.outtextxycf(200*(i mod 4)+8,43,"Operator "+decuns$(i+1),15)
end sub


sub envdraw(no,tm,sp)
v.box(744,70,999,199,159)
v.draw(744+sp,71,744+sp,198,200)
v.box(744,51,999,69,153)
v.outtextxycf(752,53,"Envelope: ",15)
end sub

sub wavedraw(no)

v.frame(199,50,712,200,0)							' clear the panel
v.box(200,51,711,69,153)
v.box(200,70,711,199,159)
v.outtextxycf(208,53,"Waveform: ",15)
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
