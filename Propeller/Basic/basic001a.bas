const _clkfreq = 336956522

'#define PSRAM4
#define PSRAM16

#ifdef PSRAM16
dim v as class using "hg009.spin2"
dim psram as class using "psram.spin2"
#endif

#ifdef PSRAM4
dim v as class using "hg009-4.spin2"
dim psram as class using "psram4.spin2"
#endif

dim kbm as class using "usbnew.spin2"
dim paula as class using "audio093b-8-sc.spin2"
'long #0: the sample phase accumulator: use it as read only although you -can- change this while playing (not recommended, the driver cog writes there at every sample)
'long #1: the current sample generated, 2 words, right:left
'long #2: the sample pointer.
'         Set bit #31 to 0 if the sample is 8 bit, 1 for 16 bit. 
'         Set bit #30 to 1 to start playing the sample from the beginning
'         Set bit #29 to 1 to synchronize channels 0 and 1 for playing stereo without the phase error
'         Set bit #28 to 1 to use interleaved samples (as in .wav file) with fractional skip enabled 
'long #3: sample loop start point
'long #4: sample loop end point. 
'         If the sample has to no loop and stop at the end, set loop start=end of sample, loop end=loop start +1 or 2 (for 16 bit)
'long #5: volume and pan
'         word #10: volume, 0..16384(=1). Values more than 16384 could cause clipping if sample is normalized
'         word #11: pan. 16384: full left, 8192: center, 0: full right
'long #6  period and skip
'         word #11: period. This is the number of Paula cycles between two samples. 
'         word #12: skip 
'         From version 0.93 it is 8.8 fixed point, so set it to 256 for 8 bit or 512 for 16-bit samples. (was: 1 and 2) 
'         Setting higher skip value skips the samples while playing, allows for higher frequencies for the same period
'long #7  was: (reserved, unused. The planned usage is ADSR stuff.)
'         Command, bit 31=set sample rate, bit 30 - set sample source (1=hub,0=psram)

#include "dir.bi"

dim audiocog,videocog as integer
dim base as ulong
dim mbox as ulong
dim ansibuf(3) as ubyte
dim textscreen (35,127) as ubyte
dim line$ as string
dim testaudio(883) as ushort



'-------------------------------------

startpsram
startvideo
let audiocog,base=paula.start(0,0,0)
waitms(1)

'dpoke base+20,0
'lpoke base+8,varptr(atari_spl) 
'lpoke base+12,1684
'lpoke base+16,1686
'dpoke base+22,8192
'dpoke base+24,79
'dpoke base+26,256
'lpoke base+28,$40000000


waitms(50)
dpoke base+20,16384
kbm.start()
kbm.mouse_set_limits(1023,575)
cls
v.setfontfamily(4)

position 4,1 : print "P2 Retromachine BASIC version 0.01"
position 4,3 : print "Ready"
position 4,4
for i=0 to 35: for j=0 to 127: textscreen(i,j)=32: next j: next i
 dim key , key2 as ulong
''--- MAIN LOOP


do

waitvbl
let key=kbm.get_key() 
let leds=kbm.ledstates() 'numlock 1 capslock 2 scrollock 4
if key>0 andalso key<4 then paula.play(0,@atari2_spl,44100,16384,0,1758): waitms(10): paula.stop(0)
if key>3 andalso key<$80000000 andalso (key and 255) <$E0 then let key2=key : let rpt=1 : let key3=key2
if key>$80000000 then let rptcnt=0 : let rpt=0
if key=0 andalso rpt=1 then rptcnt+=1
if key<$80000000 then if rptcnt=25 then key3=key2 : rptcnt=21

if key3<>0 then
  paula.play(0,@atari_spl,44100,16384,1684) 
  let key4=scantochar(key3) 
  if leds and 2 = 2 then 
    if key4>96 andalso key4<123 then
      key4-=32
    else if key4>64 andalso key4<91 then 
      key4+=32
    else if key4>22 andalso key4<32 then 
      key4-=9
    else if key4>13 andalso key4<23 then 
      key4+=39
    endif
  endif
 
  if key4>0 andalso key4<127 andalso v.cursor_x<254 then line$+=chr$(key4): textscreen(v.cursor_y,v.cursor_x/2)=key4 : v.putchar(key4)
  if key4>0 andalso key4<127 andalso v.cursor_x=254 then paula.play(0,@atari2_spl,44100,16384,0,1758): waitms(300): paula.stop(0)

  if (key3 and 255) = 43 andalso v.cursor_x>=240 then paula.play(0,@atari2_spl,44100,16384,0,1758): waitms(300): paula.stop(0)

  if (key3 and 255) = 43 andalso v.cursor_x<240 then let x=(v.cursor_x mod 16)/2: for i=x to 7: line$+=" " : textscreen(v.cursor_y,v.cursor_x/2)=32 : v.write (" ") : next i  
  if (key3 and 255) = 42 then 
      if v.cursor_x>4 then 
        line$=left$(line$,len(line$)-1): position v.cursor_x-2,v.cursor_y: v.putchar(32) : position v.cursor_x-2,v.cursor_y
      else
         line$="" : v.cursor_x=4
      endif   
   endif   
   
 ' if key3= 'tab 43, bksp 42, del 76  
 
  if key4=141 then 
    v.crlf()
    interpret(line$): line$=""
    endif 


  key3=0
  endif

loop

'-----------------------------------

sub interpret(line$)

dim part$(125)
dim i
' Pass 1: Split the line to parts


' find":" if exists

let c=instr(1,line$,":")
if c>0 then let rest$=right$(line$,len(line$)-c): line$=left$(line$,c-1)
if c=0 then rest$=""
line$=trim$(lcase$(line$))
let d$="" : let l=len(line$)
i=0 : let j=0: 

do
  do : i=i+1: let c$=mid$(line$,i,1) : let d$+=c$ :  loop until isseparator(c$) orelse i>=l
  if isseparator(c$) then i-=1 : d$=left$(d$,len(d$)-1)
  part$(j)=d$: j=j+1 : d$=""

  do : i=i+1: let c$=mid$(line$,i,1) : let d$+=c$ :  loop until ispar(c$) orelse not isseparator(c$) orelse i>=l
  if ispar(c$) andalso len(d$)=1 then part$(j)=d$ : j+=1: d$=""
  if ispar(c$) andalso len(d$)>1 then i-=1: d$=left$(d$,len(d$)-1) : part$(j)=d$: j=j+1 : d$=""
  if len(c$)>0 and not isseparator(c$) then i-=1 : d$=left$(d$,len(d$)-1): part$(j)=d$: j=j+1 : d$= "" 
  

loop until i>=l

' now remove parts that are spaces

for i=0 to j-1: part$(i)=trim$(part$(i)): next i
i=0
do 
  if len(part$(i))=0 then 
    if i=j-1 then j-=1 : exit
    if i<j-1 then for k=i to j-2 : part$(k)=part$(k+1): next k: j-=1 :  if i>0 then i-=1 
  endif
 i+=1: loop until i>=j-1


'---------------------------------------

'Pass 2: check the syntax

' the first part has to be either number o


'for i=0 to j-1: print part$(i), len(part$(i)): next i : print j

if len(part$(0))=0 then goto 102
if isint(part$(0)) then print "  This is a program line": goto 101 
let cmd=iscommand(part$(0)) 
if cmd<0 then print "  Unknown command: ";part$(0) : goto 102
let args=cargs(cmd)
' find arguments. They have to have format text,text,
' print "This is a command and it should have ";args;" arguments"
if args=0 then ' we expect eol or :
  if j=1 then execute(cmd) : goto 101 ' no more parts
  print"  Error: unexpected ";part$(1) : goto 101
else if args=1 then
  goto 101
else if args=2 then ' no eol here, j has to be at least 5
  if j<4 then print  "  Error: the command '";part$(0);"' needs 2 arguments": goto 101
  i=0: do : i+=1:   loop until part$(i)<>"("  : let lp= i-1  'skip and count leading (
  print i, : if (isnum(part$(i)) or isstr(part$(i))) then print "kwas", i 
  print i, part$(i+1), len(part$(i+1)),"lala"
  for i=1 to 5: print part$(i), : next i
  if part$(i+1)="," then print "dupa" 
  if (isnum(part$(i)) or isstr(part$(i))) and part$(i+1)="," then print "kwas" : let arg1$=part$(i): i+=2 : print i, "kwas" : goto 110 ' go find the second parameter. 
  ' If not, need to evaluate expression here ------------------------------------------------------------------------------------< TODO
110 if (isnum(part$(i)) orelse isstr(part$(i))) then 
    let arg2$=part$(i) 
    if lp>0 then 
      for k=i+1 to i+1+lp : if part$(k)<>")" then print "  Error: bad arguments" : goto 101  
      next k
    endif
  endif       
  if i>=j then print i,j, "Error: too many arguments" : goto 101
  print i,j : execute(cmd,arg1$,arg2$) : goto 101
  'parts(1) can be number, string, ( or expr. If it is number
  
  '---- todo: arguments can be expressions. We can have plot(x,y) or plot(x1 op x2 op... ,y) or plot(fun(x),y)
  
     
endif  
102 let i=1 ' here will be checked if this is an assignment
101 let i=1

print "  Ready"
v.write("  ")  
end sub

function isint(s as string) as boolean

if len(s)=0 then return false
for i=1 to len(s): let m$=mid$(s,i,1) : if m$<"0" orelse m$>"9" then return false
next i
return true
end function

function isnum(s as string) as boolean

dim i
if len(s)=0 then return false
let dots=0: let es=0
for i=1 to len(s): let m$=mid$(s,i,1) 
if i=1 then if (m$<"0" orelse m$>"9") andalso m$<>"." andalso m$<>"$" andalso m$<>"%" then return false ' first char can be $ for hex, % for bin, or digit or .
if i>1 then if (m$<"0" orelse m$>"9") andalso m$<>"." andalso m$<>"_" andalso ucase$(m$)<>"E" then return false           ' next chars can be digit, . , E and _
if m$="." then dots+=1 : if dots>1 then return false     ' only one dot allowed
if ucase$(m$)="E" then es+=1 : if es>1 then return false ' only one E allowed
next i
if ucase$(m$)="E" then return false ' e cannot be the last char in number
return true
end function

function isstr (s as string) as boolean

if len(s)=0 then return false
if left$(s,1)<>chr$(34)then return false
if right$(s,1)<>chr$(34)then return false
return true
end function

function ispar(s as string) as boolean

if s="(" orelse s=")" then return true else return false
end function

function isseparator(s as string) as boolean

if s=" " orelse s=":" orelse s="(" orelse s="=" orelse s="+" orelse s="-" orelse s="*" orelse s="/" orelse s=")" orelse s="," then return true else return false
end function

function iscommand(s as string) as integer
for i=0 to maxcommand: if s=command(i) then return i
next i
return -1
end function

sub execute(cmd,arg1$="", arg2$="", arg3$="", arg4$="")

select case cmd
case 0              'cls
cls:print ""
case 1		    'new
cls: position 4,1 : print "P2 Retromachine BASIC version 0.01" 
print " "   ' todo: clear all program structures
end select
end sub
'----------------------------------
sub startpsram
psram.startx(0, 0, 11, -1)
mbox=psram.getMailbox(0)
end sub

sub startaudio
audiocog,base=paula.start(mbox,$75A00,$7A400)
end sub 

sub stopaudio
cpustop(audiocog)
audiocog=-1
end sub

sub cls(fg=154,bg=147)
v.cls(fg,bg)
end sub

function startvideo(mode=64, pin=0, mb=0) 'todo return a cog#
dim videocog as ulong
videocog=v.start(pin,mbox)

for thecog=0 to 7:psram.setQos(thecog, 112 << 16) :next thecog
psram.setQoS(videocog, $7FFFf400) 
open SendRecvDevice(@v.putchar, nil, nil) as #0
return videocog
end function


#define plot v.plot1

sub pslpoke(addr as ulong,value as ulong)
psram.filllongs(addr,value,1,0)
end sub

sub pspoke(addr as ulong,value as ulong)
psram.fillbytes(addr,value,1,0)
end sub

function pspeek(adr as ulong) as ubyte
dim res as ubyte
psram.read1(addr(res),adr,1)
return res
end function

function pslpeek(adr as ulong) as ulong
dim res as ulong
psram.read1(addr(res),adr,4)
return res
end function

sub position(x,y)
v.setcursorpos(x,y)
end sub

sub waitvbl
  v.waitvbl(1)
end sub

function scantochar(key)

select case (key shr 8) and 255
case 0
return keys(4*(key and 255))
case 2,32
return keys(4*(key and 255)+1)
case 64
return keys(4*(key and 255)+2)
case 66,96
return keys(4*(key and 255)+3)

end select

end function
const maxcommand=8
dim shared as string command(maxcommand)={_
"cls","new","plot","draw","print","circle","fcircle","box","frame"}
dim shared as integer cargs(maxcommand)={
 0,0,2,2,-1,3,3,4,4} ' these are argument number for commands. If -1, it is not defined, if from..to , low byte is to, high byte is from



dim shared as ubyte keys(1023)={
 0,0,0,0, 			'0
 0,0,0,0,_
 0,0,0,0,_
 0,0,0,0,_
 97,65,23,14,_
 98,66,0,0,_
 99,67,25,16,_
 100,68,0,0,_
 101,69,24,15,_
 102,70,0,0,_
 103,71,0,0,_
 104,72,0,0,_
 105,73,0,0,_
 106,74,0,0,_
 107,75,0,0,_
108,76,31,22,_
109,77,0,0,_
110,78,26,17,_
111,79,30,21,_
112,80,0,0,_
113,81,0,0,_
114,82,0,0,_
115,83,27,18,_
116,84,0,0,_
117,85,0,0,_
118,86,0,0,_
119,87,0,0,_
120,88,28,19,_
121,89,0,0,_
122,90,29,20,_
49,33,4,0,_
50,64,5,0,_
51,35,6,0,_
52,36,7,0,_
53,37,8,0,_
54,94,9,0,_
55,38,10,0,_
56,42,11,0,_
57,40,12,0,_
48,41,13,0,_
141,141,0,0,_
155,155,0,0,_
136,136,0,0,_
137,137,0,0,_
32,32,0,0,_
45,95,0,0,_
61,43,0,0,_
91,123,0,0,_
93,125,0,0,_
92,124,0,0,_
35,126,0,0,_
59,58,0,0,_
39,34,0,0,_
96,126,3,0,_
44,60,0,0,_
46,62,0,0,_
47,63,0,0,_
185,185,0,0,_
186,0,0,0,_
187,0,0,0,_
188,0,0,0,_
189,0,0,0,_
190,0,0,0,_
191,0,0,0,_
192,0,0,0,_
193,0,0,0,_
194,0,0,0,_
195,0,0,0,_
196,0,0,0,_
197,0,0,0,_
198,0,0,0,_
199,0,0,0,_
200,0,0,0,_
201,0,0,0,_
202,0,0,0,_
203,0,0,0,_
127,127,0,0,_
204,0,0,0,_
205,0,0,0,_
206,0,0,0,_
207,0,0,0,_
208,0,0,0,_
209,0,0,0,_
210,0,0,0,_
47,47,0,0,_
42,42,0,0,_
45,45,0,0,_
43,43,0,0,_
141,141,0,0,_
49,49,0,0,_
50,50,0,0,_
51,51,0,0,_
52,52,0,0,_
53,53,0,0,_
54,54,0,0,_
55,55,0,0,_
56,56,0,0,_
57,57,0,0,_
48,48,0,0,_
46,127,0,0,_
92,124,0,0,_
0,0,0,0,_
0,0,0,0,_
61,61,0,0,_
0,0,0,0,_
0,0,0,0,_
0,0,0,0,_
0,0,0,0,_
0,0,0,0,_
0,0,0,0,_
0,0,0,0,_
0,0,0,0,_
0,0,0,0,_
0,0,0,0,_
0,0,0,0,_
0,0,0,0,_
0,0,0,0,_
0,0,0,0,_
0,0,0,0,_
0,0,0,0,_
0,0,0,0,_
0,0,0,0,_
0,0,0,0,_
0,0,0,0,_
0,0,0,0,_
0,0,0,0,_
0,0,0,0,_
0,0,0,0,_
0,0,0,0,_
0,0,0,0,_
0,0,0,0,_
0,0,0,0,_
0,0,0,0,_
44,44,0,0,_
0,0,0,0,_
0,0,0,0,_
0,0,0,0,_
0,0,0,0,_
0,0,0,0,_
0,0,0,0,_
0,0,0,0,_
0,0,0,0,_
0,0,0,0,_
0,0,0,0,_
0,0,0,0,_
0,0,0,0,_
0,0,0,0,_
0,0,0,0,_
0,0,0,0,_
0,0,0,0,_
0,0,0,0,_
0,0,0,0,_
0,0,0,0,_
0,0,0,0,_
0,0,0,0,_
0,0,0,0,_
0,0,0,0,_
0,0,0,0,_
0,0,0,0,_
0,0,0,0,_
0,0,0,0,_
0,0,0,0,_
0,0,0,0,_
0,0,0,0,_
0,0,0,0,_
0,0,0,0,_
0,0,0,0,_
0,0,0,0,_
0,0,0,0,_
0,0,0,0,_
0,0,0,0,_
0,0,0,0,_
0,0,0,0,_
0,0,0,0,_
0,0,0,0,_
0,0,0,0,_
0,0,0,0,_
0,0,0,0,_
0,0,0,0,_
0,0,0,0,_
0,0,0,0,_
0,0,0,0,_
0,0,0,0,_
0,0,0,0,_
0,0,0,0,_
0,0,0,0,_
0,0,0,0,_
0,0,0,0,_
0,0,0,0,_
0,0,0,0,_
0,0,0,0,_
0,0,0,0,_
0,0,0,0,_
0,0,0,0,_
0,0,0,0,_
0,0,0,0,_
0,0,0,0,_
0,0,0,0,_
0,0,0,0,_
0,0,0,0,_
0,0,0,0,_
0,0,0,0,_
0,0,0,0,_
0,0,0,0,_
0,0,0,0,_
0,0,0,0,_
0,0,0,0,_
0,0,0,0,_
0,0,0,0,_
0,0,0,0,_
0,0,0,0,_
0,0,0,0,_
0,0,0,0,_
0,0,0,0,_
0,0,0,0,_
0,0,0,0,_
0,0,0,0,_
0,0,0,0,_
0,0,0,0,_
0,0,0,0,_
0,0,0,0,_
0,0,0,0,_
0,0,0,0,_
0,0,0,0,_
0,0,0,0,_
0,0,0,0,_
0,0,0,0,_
0,0,0,0,_
0,0,0,0,_
0,0,0,0,_
0,0,0,0,_
0,0,0,0,_
0,0,0,0,_
0,0,0,0,_
0,0,0,0,_
0,0,0,0,_
0,0,0,0,_
0,0,0,0,_
0,0,0,0,_
0,0,0,0,_
0,0,0,0,_
0,0,0,0,_
0,0,0,0,_
0,0,0,0,_
0,0,0,0,_
0,0,0,0,_
0,0,0,0,_
0,0,0,0,_
0,0,0,0,_
0,0,0,0,_
0,0,0,0,_
0,0,0,0,_
0,0,0,0,_
0,0,0,0,_
0,0,0,0,_
0,0,0,0}

const key_enter=141    ' //USB_HID_BOOT_USAGE_ID[40,0];    //141;
const   key_escape=155    '//USB_HID_BOOT_USAGE_ID[41,0];    //155;
const   key_backspace=136 '//USB_HID_BOOT_USAGE_ID[42,0];    //136;
const   key_tab=137       '/'/USB_HID_BOOT_USAGE_ID[43,0];    //137;
const   key_f1=186        '//USB_HID_BOOT_USAGE_ID[58,0];    //186;
const   key_f2=187        '//USB_HID_BOOT_USAGE_ID[59,0];    //187;
const   key_f3=188        ''//USB_HID_BOOT_USAGE_ID[60,0];    //188;
const   key_f4=189        '//USB_HID_BOOT_USAGE_ID[61,0];    //189;
const   key_f5=190        '//USB_HID_BOOT_USAGE_ID[62,0];    //190;
const   key_f6=191        '//USB_HID_BOOT_USAGE_ID[63,0];    //191;
const   key_f7=192        '//USB_HID_BOOT_USAGE_ID[64,0];    //192;
const   key_f8=193        '//USB_HID_BOOT_USAGE_ID[65,0];    //193;
const   key_f9=194        '//USB_HID_BOOT_USAGE_ID[66,0];    //194;
const   key_f10=195      ' //USB_HID_BOOT_USAGE_ID[67,0];    //195;
const   key_f11=196       '//USB_HID_BOOT_USAGE_ID[68,0];    //196;
const   key_f12=197       '//USB_HID_BOOT_USAGE_ID[69,0];    //197;
const   key_rightarrow=206'//USB_HID_BOOT_USAGE_ID[79,0];    //206;
const   key_leftarrow=207 '//USB_HID_BOOT_USAGE_ID[80,0];    //207;
const   key_downarrow=208 '//USB_HID_BOOT_USAGE_ID[81,0];    //208;
const   key_uparrow=209   '//USB_HID_BOOT_USAGE_ID[82,0];    //209;

asm shared
atari_spl file "atari.spl"
atari2_spl file "atari2.spl" '1758
end asm
