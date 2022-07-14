'-----------------------------------------------------------------------
' Frequency modulation sound toy
' v. 0.04 - 20220711
' pik33@o2.pl
'-----------------------------------------------------------------------

'sample rate=82264.77587890625

#include "retromachine.bi"

const version$="Propeller 2 FM synth v.0.06"


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
dim oct(7)
dim r1,r2,r3,r4,l1,l2,l3,l4,lv,freq,feedback as ulong(7)
dim slider1,slider2,slider3,slider4,slider5,slider6,slider7,slider8,slider9 as ulong
dim knob1,knob2,knob3,knob4,knob5,knob6,knob7,knob8,knob9 as ulong
dim switch1,switch2,switch3,switch4,switch5,switch6,switch7,switch8,switch9 as ulong
dim operator, ctrlstate, kbdpressed, base2, change as ulong

dim channelassign(31)
dim channelnotes(31)
dim midi as ulong
declare midibytes alias midi as ubyte(3)

' ----------- start cogs

startmidi()
startvideo()
startaudio()

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
  if i=0 then lv(i)=$FFFF else lv(i)=0
  
next i
operator=0: 'change=1
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
lpoke 2304+40*i+base+00,r1(i)
lpoke 2304+40*i+base+04,$1200000
lpoke 2304+40*i+base+08,$1200000
lpoke 2304+40*i+base+12,$1200000 ' 0.7 ms
lpoke 2304+40*i+base+16,$7FFF_FFFF
lpoke 2304+40*i+base+20,$7FFF_0000
lpoke 2304+40*i+base+24,$7FFF_0000
lpoke 2304+40*i+base+28,$3FFF_FFFF
lpoke 2304+40*i+base+32,$3FFF_FFFF
lpoke 2304+40*i+base+36,$3FFF_FFFF
next i
 lpoke base+2160+96, lpeek(base+2160+96) + $20000000  
'------- prepare the UI

preparepanels
kbddraw
waitms(1000)
'---------------------------------------------------------------------------------------
'------------------------------- THE MAIN LOOP -----------------------------------------
'---------------------------------------------------------------------------------------

90 
if change=1 then
  if ctrlstate=0 then 
 ' print operator,r1(operator)
    r1(operator)=round(61*exp(slider1/10.0)) 
    r2(operator)=round(61*exp(slider2/10.0))
    r3(operator)=round(61*exp(slider3/10.0))
    r4(operator)=round(61*exp(slider4/10.0)) 
    l1(operator)=$43FF_FFFF-$1C+$78F1E4*slider5  
    l2(operator)=$43FF_FFFF-$1C+$78F1E4*slider6  
    l3(operator)=$43FF_FFFF-$1C+$78F1E4*slider7  
    l4(operator)=$43FF_FFFF-$1C+$78F1E4*slider8
    feedback(operator)=round(exp(knob1/11.4514))

 
    lpoke 2304+40*operator+base+00,r1(operator)
    lpoke 2304+40*operator+base+04,r2(operator)
    lpoke 2304+40*operator+base+08,r3(operator)
    lpoke 2304+40*operator+base+12,r4(operator)
    lpoke 2304+40*operator+base+16,l1(operator)
    lpoke 2304+40*operator+base+20,l2(operator)
    lpoke 2304+40*operator+base+24,l3(operator)
    lpoke 2304+40*operator+base+28,l4(operator)
    lpoke 2304+40*operator+base+32,feedback(operator)
    lpoke 2304+40*operator+base+36,$0
  
    lpoke base+2160+96, lpeek(base+2160+96) + $20000000  ' tell the audio driver to read new data
  endif
  change=0
endif 

midi=rm.readmidi()
if midi<>0 then position 0,29 : v.write ("Midi command: "): v.write(v.inttohex(midi,8)) 
position 30,29: v.write("Channel time: "): v.write(v.inttostr2(lpeek($70),3))
if midi=0 then goto 90
let b3=midibytes(3): let b0=midibytes(0): let b1=midibytes(1) : let b2=midibytes(2)

'----- MIDI control change

if b3=$B0 then 

  if b1=control00 then slider1=b0: change=1 	' rate 1 at slider 1
  if b1=control01 then slider2=b0: change=1	' rate 2 at slider 2
  if b1=control02 then slider3=b0: change=1	' rate 3 at slider 3
  if b1=control03 then slider4=b0: change=1	' rate 4 st slider 4
  if b1=control04 then slider5=b0: change=1	' level 1 at slider 5
  if b1=control05 then slider6=b0: change=1	' level 2 at slider 6
  if b1=control06 then slider7=b0: change=1	' level 3 at slider 7
  if b1=control07 then slider8=b0: change=1	' level 4 at slider 8
  if b1=control08 then slider9=b0 : lv(operator)=round(exp(slider9/11.4514)) ' Overall operator level, change=0 as it will be serviced at the main cog 
  if b1=control09 then knob1=b0:   change=1	' Feedback level
  if b1=control10 then knob2=b0  	        ' Frequency coarse level, serviced at the main cog
  if b1=control11 then knob2=b0  	        ' Frequency fine level, serviced at the main cog
  refreshpanel(operator)
  b3=0
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
  lpoke base2+100,round(notes(b1)*freqv)         		' set a new frequency
  lpoke base2+96,$4000_0000+(b0*lv(0))/2048				' set a new volume and trigger the note on ' todo: rest ops are to set!!!
  channelassign(minc)=kbdpressed: kbdpressed+=1 		' update the channel "time" (in key presses)
  b3=0
  kbdnoteon(b1)
  goto 90
endif

' ----- MIDI note off 

if b3=$80 orelse (b3=$90 andalso b0=0) then
  for i=0 to maxchannel
    if channelnotes(i)=b1 then lpoke base+144*i+96,lpeek(base+144*i+96) + $8000_0000 ' find the channel and trigger the note off
  next i  
  b3=0
  kbdnoteoff(b1)
  goto 90
endif

goto 90

'---------------------------------------------------------------------------------------
'------------------------------- The main loop END -------------------------------------
'---------------------------------------------------------------------------------------

'------------------ Prepare the UI

sub preparepanels

' 1. Channel and oscilloscope panel at graphic canvas

cls(15,82)
v.setbordercolor2(88)	
v.setfontfamily(4)						'
v.outtextxycz((800-16*len(version$))/2,4,version$,92,82,2,2)
v.setfontfamily(0)							'

for i=0 to 3
  v.box(5+200*i,41,196+200*i,59,26+16*i)
  v.box(5+200*i,60,196+200*i,194,19+16*i)
  v.outtextxycf(8+200*i,64,"R1 ",26+16*i) :	v.outtextxycf(32+200*i,64,v.inttostr2(slider1,3),29+16*i)
  v.outtextxycf(8+200*i,80,"R2 ",26+16*i) :	v.outtextxycf(32+200*i,80,v.inttostr2(slider2,3),29+16*i)
  v.outtextxycf(8+200*i,96,"R3 ",26+16*i) :	v.outtextxycf(32+200*i,96,v.inttostr2(slider3,3),29+16*i)
  v.outtextxycf(8+200*i,112,"R4 ",26+16*i) :	v.outtextxycf(32+200*i,112,v.inttostr2(slider4,3),29+16*i)
  v.outtextxycf(8+200*i,128,"L1 ",26+16*i) :	v.outtextxycf(32+200*i,128,v.inttostr2(slider5,3),29+16*i)
  v.outtextxycf(8+200*i,144,"L2 ",26+16*i) :	v.outtextxycf(32+200*i,144,v.inttostr2(slider6,3),29+16*i)
  v.outtextxycf(8+200*i,160,"L3 ",26+16*i) :	v.outtextxycf(32+200*i,160,v.inttostr2(slider7,3),29+16*i)
  v.outtextxycf(8+200*i,176,"L4 ",26+16*i) :	v.outtextxycf(32+200*i,176,v.inttostr2(slider8,3),29+16*i)
  v.outtextxycf(72+200*i,64,"Lvl ",26+16*i) :   v.outtextxycf(112+200*i,64,v.inttostr2(slider9,3),29+16*i)
  v.outtextxycf(72+200*i,80,"Freq",26+16*i) :   v.outtextxycf(112+200*i,80,v.inttostr2(knob5,3),29+16*i)
  v.outtextxycf(72+200*i,96,"Feed",26+16*i) :	v.outtextxycf(112+200*i,96,v.inttostr2(knob1,3),29+16*i)
next i

v.outtextxycf(8,43,"Operator 1 ",15)
v.outtextxycf(8+200,43,"Operator 2 ",0)
v.outtextxycf(8+400,43,"Operator 3 ",0)
v.outtextxycf(8+600,43,"Operator 4 ",0)

end sub

sub refreshpanel(i)
v.outtextxycg(32+200*i,64,v.inttostr2(slider1,3),29+16*i,19+16*i)
v.outtextxycg(32+200*i,80,v.inttostr2(slider2,3),29+16*i,19+16*i)
v.outtextxycg(32+200*i,96,v.inttostr2(slider3,3),29+16*i,19+16*i)
v.outtextxycg(32+200*i,112,v.inttostr2(slider4,3),29+16*i,19+16*i)
v.outtextxycg(32+200*i,128,v.inttostr2(slider5,3),29+16*i,19+16*i)
v.outtextxycg(32+200*i,144,v.inttostr2(slider6,3),29+16*i,19+16*i)
v.outtextxycg(32+200*i,160,v.inttostr2(slider7,3),29+16*i,19+16*i)
v.outtextxycg(32+200*i,176,v.inttostr2(slider8,3),29+16*i,19+16*i)
v.outtextxycg(112+200*i,64,v.inttostr2(slider9,3),29+16*i,19+16*i)
v.outtextxycg(112+200*i,80,v.inttostr2(knob5,3),29+16*i,19+16*i)
v.outtextxycg(112+200*i,96,v.inttostr2(knob1,3),29+16*i,19+16*i)
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

sub kbddraw

for o=0 to 8
  for n=0 to 6
    if o=10 andalso n>3 then continue
    v.frame(20+84*o+12*n,360,32+84*o+12*n,450,0)
    v.box(21+84*o+12*n,361,31+84*o+12*n,449,15)
  
    'v.box(8+12*(8*7+n)+1,361,20+12*(7*o+n)-1,449,15)
  next n
  v.box(28+12*(7*o),360,36+12*(7*o),410,0)
  v.box(40+12*(7*o),360,48+12*(7*o),410,0)
  v.box(64+12*(7*o),360,72+12*(7*o),410,0)
  v.box(76+12*(7*o),360,84+12*(7*o),410,0)
  v.box(88+12*(7*o),360,96+12*(7*o),410,0)
  v.outtextxycf(23+84*o,434,v.inttostr(o+1),0)
next o 

end sub

sub kbdnoteon(note)

dim n,o,x,y
if note<$18 then return
o=b1/12-2: n=b1 mod 12: x=20+84*o
select case n
  case 0
    v.box(x+2,425,x+10,433,248)
  case 1
    v.box(x+10,395,x+14,405,248)
  case 2
    v.box(x+12+2,425,x+12+10,433,248)
  case 3
    v.box(x+12+10,395,x+12+14,405,248)
  case 4
    v.box(x+24+2,425,x+24+10,433,248)
  case 5
    v.box(x+36+2,425,x+36+10,433,248)
  case 6
    v.box(x+36+10,395,x+36+14,405,248)
  case 7
    v.box(x+48+2,425,x+48+10,433,248)
  case 8
    v.box(x+48+10,395,x+48+14,405,248)
  case 9
    v.box(x+60+2,425,x+60+10,433,248)
  case 10
    v.box(x+60+10,395,x+60+14,405,248)
  case 11
    v.box(x+72+2,425,x+72+10,433,248)  
  end select
end sub

sub kbdnoteoff(note)

dim n,o,x,y
if note<$18 then return
o=b1/12-2: n=b1 mod 12: x=20+84*o
select case n
  case 0
    v.box(x+2,425,x+10,433,15)
  case 1
    v.box(x+10,395,x+14,405,0)
  case 2
    v.box(x+12+2,425,x+12+10,433,15)
  case 3
    v.box(x+12+10,395,x+12+14,405,0)
  case 4
    v.box(x+24+2,425,x+24+10,433,15)
  case 5
    v.box(x+36+2,425,x+36+10,433,15)
  case 6
    v.box(x+36+10,395,x+36+14,405,0)
  case 7
    v.box(x+48+2,425,x+48+10,433,15)
  case 8
    v.box(x+48+10,395,x+48+14,405,0)
  case 9
    v.box(x+60+2,425,x+60+10,433,15)
  case 10
    v.box(x+60+10,395,x+60+14,405,0)  
  case 11
    v.box(x+72+2,425,x+72+10,433,15)
  
  end select
end sub
