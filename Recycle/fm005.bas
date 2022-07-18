'-----------------------------------------------------------------------
' Frequency modulation sound toy
' v. 0.04 - 20220711
' pik33@o2.pl
'-----------------------------------------------------------------------

'sample rate=82264.77587890625

#include "retromachine.bi"

const version$="Propeller 2 FM synth v.0.03"


const c212=1.05946309435929526456
const c_1=8.17579891564370733371
const skipv=52209.06822*2 /16 '(*2 for debug until optimized, sampling at 41k) /16 for shl4
const adsrv=52209 ' for 1 s
const maxchannel=15


dim envnames$(31)
dim wavenames$(63)

dim notes(127) as single 
dim oct(7)
dim r1,r2,r3,r4,l1,l2,l3,l4,lv,freq,feedback as ulong(7)
dim slider1,slider2,slider3,slider4,slider5,slider6,slider7,slider8,slider9 as ulong
dim knob1,knob2,knob3,knob4,knob5,knob6,knob7,knob8,knob9 as ulong
dim switch1,switch2,switch3,switch4,switch5,switch6,switch7,switch8,switch9 as ulong
dim operator as ulong

dim channelassign(31)
dim channelnotes(31)
dim midi as ulong
declare midibytes alias midi as ubyte(3)

startmachine
startpsram
startvideo
startaudio

let ctrlstate=0 ' ADSR via sliders

 
'var aaaa=$88000000

'asm
' qexp aaaa
' getqx aaaa
'end asm

'print aaaa
 
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

next i


operator=0: 'change=1


preparepanels
kbddraw 


 

let f#=c_1: for i=0 to 127: notes(i)=f# : f#=f#*c212: next i


for i=0 to 15

lpoke 144*i+base+00,0
lpoke 144*i+base+04,0
lpoke 144*i+base+08,%0111111111111
lpoke 144*i+base+12,0 ' mod

lpoke 144*i+base+16,0
lpoke 144*i+base+20,0
lpoke 144*i+base+24,%0111111111111
lpoke 144*i+base+28,0 ' mod

lpoke 144*i+base+32,0
lpoke 144*i+base+36,0
lpoke 144*i+base+40,%0111111111111
lpoke 144*i+base+44,0 ' mod

lpoke 144*i+base+48,0
lpoke 144*i+base+52,0
lpoke 144*i+base+56,%0111111111111
lpoke 144*i+base+60,0 ' mod

lpoke 144*i+base+64,0
lpoke 144*i+base+68,0
lpoke 144*i+base+72,%0111111111111
lpoke 144*i+base+76,0 ' mod

lpoke 144*i+base+80,0
lpoke 144*i+base+84,0
lpoke 144*i+base+88,%0111111111111
lpoke 144*i+base+92,0 ' mod


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
lpoke 2304+40*i+base+00,$1200000 ' rate 99=2
lpoke 2304+40*i+base+04,$1200000
lpoke 2304+40*i+base+08,$1200000
lpoke 2304+40*i+base+12,$1200000 ' 0.7 ms
lpoke 2304+40*i+base+16,$7FFF_FFFF
lpoke 2304+40*i+base+20,$7FFF_0000
lpoke 2304+40*i+base+24,$7FFF_0000
lpoke 2304+40*i+base+28,$3FFF_FFFF
lpoke 2304+40*i+base+32,$3FFF_FFFF
lpoke 2304+40*i+base+36,$3FFF_FFFF

change=1

next i



for i=0 to maxchannel: channelassign(i)=0 : next i
let kbdpressed=1


90 position 30,35: v.write(v.inttostr2(lpeek($70),3))
if change=1 then
  if ctrlstate=0 then 
  r1(operator)=round(61*exp(slider1/10.0)) 
  r2(operator)=round(61*exp(slider2/10.0))
  r3(operator)=round(61*exp(slider3/10.0))
  r4(operator)=round(61*exp(slider4/10.0)) 

  l1(operator)=$43FF_FFFF-$1C+$78F1E4*slider5  
  l2(operator)=$43FF_FFFF-$1C+$78F1E4*slider6  
  l3(operator)=$43FF_FFFF-$1C+$78F1E4*slider7  
  l4(operator)=$43FF_FFFF-$1C+$78F1E4*slider8
  
  feedback(operator)=round(exp(knob1/11.5))
  refreshpanel(operator)
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
  

  lpoke base+2160+96, lpeek(base+2160+96) + $20000000 

  endif
  change=0
endif 


midi=rm.readmidi():if midi<>0 then position 0,35: v.write(v.inttohex(midi,8))
if midi=0 then goto 90
let b3=midibytes(3): let b0=midibytes(0): let b1=midibytes(1) : let b2=midibytes(2)

if b3=$B0 andalso b1=$29 then  let slider1=b0: let change=1 	' Novation Impulse slider #1
if b3=$B0 andalso b1=$2A then  let slider2=b0: let change=1	' Novation Impulse slider #2
if b3=$B0 andalso b1=$2B then  let slider3=b0: let change=1	' Novation Impulse slider #3
if b3=$B0 andalso b1=$2C then  let slider4=b0: let change=1	' Novation Impulse slider #4
if b3=$B0 andalso b1=$2D then  let slider5=b0: let change=1	' Novation Impulse slider #5
if b3=$B0 andalso b1=$2E then  let slider6=b0: let change=1	' Novation Impulse slider #6
if b3=$B0 andalso b1=$2F then  let slider7=b0: let change=1	' Novation Impulse slider #7
if b3=$B0 andalso b1=$30 then  let slider8=b0: let change=1	' Novation Impulse slider #8
if b3=$B0 andalso b1=$30 then  let slider9=b0: let change=1	' Novation Impulse slider #9
if b3=$B0 andalso b1=$15 then  let knob1=b0: let change=1	' Novation Impulse knob #1, controller #$15: set envelope
if b3=$B0 andalso b1=$16 then  let knob2=b0: let change=1	' Novation Impulse knob #1, controller #$15: set envelope
if b3=$B0 andalso b1=$17 then  let knob3=b0: let change=1	' Novation Impulse knob #1, controller #$15: set envelope
if b3=$B0 andalso b1=$18 then  let knob4=b0: let change=1	' Novation Impulse knob #1, controller #$15: set envelope
if b3=$B0 andalso b1=$19 then  let knob5=b0: let change=1	' Novation Impulse knob #1, controller #$15: set envelope
if b3=$B0 andalso b1=$1a then  let knob6=b0: let change=1	' Novation Impulse knob #1, controller #$15: set envelope
if b3=$B0 andalso b1=$1b then  let knob7=b0: let change=1	' Novation Impulse knob #1, controller #$15: set envelope
if b3=$B0 andalso b1=$1b then  let knob8=b0: let change=1	' Novation Impulse knob #1, controller #$15: set envelope

 

if b3=$90 andalso b0<>0 then
  let min=$7FFFFFFF: let minc=0
  for i=0 to maxchannel
    if channelassign(i)<min then min=channelassign(i): minc=i 
  next i
  channelnotes(minc)=b1
  let base2=base+144*minc
  let skip=round(notes(b1)*skipv) : 
  lpoke base2+100,skip
  lpoke base2+96,$4000_0000+b0*32
  channelassign(minc)=kbdpressed: kbdpressed+=1 
  b3=0
  goto 90
endif

if b3=$80 orelse (b3=$90 andalso b0=0) then
  for i=0 to maxchannel
    if channelnotes(i)=b1 then lpoke base+144*i+96,lpeek(base+144*i+96) + $8000_0000
  next i  
  b3=0
  goto 90
  
endif

goto 90


sub preparepanels

' 1. Channel and oscilloscope panel at graphic canvas

cls(15,144)	
v.setfontfamily(4)						'
v.outtextxycz((1024-16*len(version$))/2,4,version$,200,0,2,2)
v.setfontfamily(0)							'



for i=0 to 1

v.frame(4+256*i,50,252+256*i,205,0)							' clear the panel
v.box(5+256*i,51,251+256*i,69,154+16*i)
v.box(5+256*i,70,251+256*i,204,147+16*i)

v.outtextxycf(8+256*i,74,"R1 ",154+16*i) :		v.outtextxycf(32+256*i,74,v.inttostr2(slider1,3),157+16*i)
v.outtextxycf(8+256*i,90,"R2 ",154+16*i) :		v.outtextxycf(32+256*i,90,v.inttostr2(slider2,3),157+16*i)
v.outtextxycf(8+256*i,106,"R3 ",154+16*i) :	v.outtextxycf(32+256*i,106,v.inttostr2(slider3,3),157+16*i)
v.outtextxycf(8+256*i,122,"R4 ",154+16*i) :	v.outtextxycf(32+256*i,122,v.inttostr2(slider4,3),157+16*i)
v.outtextxycf(8+256*i,138,"L1 ",154+16*i) :	v.outtextxycf(32+256*i,138,v.inttostr2(slider5,3),157+16*i)
v.outtextxycf(8+256*i,154,"L2 ",154+16*i) :	v.outtextxycf(32+256*i,154,v.inttostr2(slider6,3),157+16*i)
v.outtextxycf(8+256*i,170,"L3 ",154+16*i) :	v.outtextxycf(32+256*i,170,v.inttostr2(slider7,3),157+16*i)
v.outtextxycf(8+256*i,186,"L4 ",154+16*i) :	v.outtextxycf(32+256*i,186,v.inttostr2(slider8,3),157+16*i)
v.outtextxycf(96+256*i,74,"Level ",154+16*i) :    	v.outtextxycf(168+256*i,74,v.inttostr2(slider9,3),157+16*i)
v.outtextxycf(96+256*i,90,"Freq",154+16*i) :      	v.outtextxycf(168+256*i,90,v.inttostr2(knob5,3),157+16*i)
v.outtextxycf(96+256*i,106,"Feedback",154+16*i) :  v.outtextxycf(168+256*i,106,v.inttostr2(knob1,3),157+16*i)

next i
v.outtextxycf(8,53,"Operator 1 ",15)
v.outtextxycf(8+256,53,"Operator 2 ",0)

end sub

sub refreshpanel(i)

v.outtextxycg(32+256*i,74,v.inttostr2(slider1,3),157+16*i,147+16*i)
v.outtextxycg(32+256*i,90,v.inttostr2(slider2,3),157+16*i,147+16*i)
v.outtextxycg(32+256*i,106,v.inttostr2(slider3,3),157+16*i,147+16*i)
v.outtextxycg(32+256*i,122,v.inttostr2(slider4,3),157+16*i,147+16*i)
v.outtextxycg(32+256*i,138,v.inttostr2(slider5,3),157+16*i,147+16*i)
v.outtextxycg(32+256*i,154,v.inttostr2(slider6,3),157+16*i,147+16*i)
v.outtextxycg(32+256*i,170,v.inttostr2(slider7,3),157+16*i,147+16*i)
v.outtextxycg(32+256*i,186,v.inttostr2(slider8,3),157+16*i,147+16*i)
v.outtextxycg(168+256*i,74,v.inttostr2(slider9,3),157+16*i,147+16*i)
v.outtextxycg(168+256*i,90,v.inttostr2(knob5,3),157+16*i,147+16*i)
v.outtextxycg(168+256*i,106,v.inttostr2(knob1,3),157+16*i,147+16*i)
end sub


sub envdraw(no,tm,sp)
v.box(744,70,999,199,159)
v.draw(744+sp,71,744+sp,198,192)
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

for o=0 to 10
  for n=0 to 7
    if o=10 andalso n>3 then continue
    v.frame(8+12*(8*7+n),400,20+12*(7*o+n),500,0)
  next n
  v.box(16+12*(7*o),400,24+12*(7*o),460,0)
  v.box(28+12*(7*o),400,36+12*(7*o),460,0)
  v.box(52+12*(7*o),400,60+12*(7*o),460,0)
  v.box(64+12*(7*o),400,72+12*(7*o),460,0)
  v.box(76+12*(7*o),400,84+12*(7*o),460,0)

next o 


end sub
