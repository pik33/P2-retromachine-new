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


#include "dir.bi"

dim audiocog,videocog as integer
dim base as ulong
dim mbox as ulong
dim ansibuf(3) as ubyte
dim textscreen (35,127) as ubyte
dim line$ as string
dim testaudio(883) as ushort

class part
  dim part$ as string
  dim token as integer
  dim priority as integer
end class

type parts as part(125) 
dim lparts as parts

union eresult
  dim iresult as integer
  dim uresult as ulong
  dim lresult as longint
  dim ulresult as ulongint
  dim fresult as double
  dim sresult as string
  end union

class expr_result
dim result as eresult
dim result_type as ubyte ' 0 i 1 u 2 i64 3 u64 4f 5s 6 error
end class


'-------------------------------------

startpsram
startvideo
let audiocog,base=paula.start(0,0,0)
waitms(1)



waitms(50)
dpoke base+20,16384
kbm.start()
kbm.mouse_set_limits(1023,575)
cls
v.setfontfamily(4)

position 4,1 : print "P2 Retromachine BASIC version 0.03"
position 4,3 : print "Ready"
position 4,4
for i=0 to 35: for j=0 to 127: textscreen(i,j)=32: next j: next i
dim key , key2 as ulong
 
 
const token_plus=1
const token_minus=2
const token_or=3
const token_xor=4
const token_mul=5
const token_fdiv=6
const token_and=7
const token_div=8
const token_mod=9
const token_shl=10
const token_shr=11
const token_power=12
const token_not=13
const token_at=14
const token_inc=15
const token_dec=16
const token_comma=17
const token_semicolon=18
const token_ear=19
const token_rpar=20
const token_lpar=21
const token_decimal=512
const token_integer=513
const token_float=514
const token_string=515
const token_name=515


dim ct as integer
 
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

 
dim i,j,k,q
dim result as expr_result
dim etype

' Pass 1: Split the line to parts, detect and concatenate strings

dim separators(125): for i=0 to 125: separators(i)=0 :next i
for i=0 to 125: lparts(i).part$="": next i

' 1a : extract the first command, split the line to first command and the rest

line$=trim$(lcase$(line$)):let d$="" : let l=len(line$)
let d=instr(1,line$,":"): if d>0 then let rest$=right$(line$,len(line$)-d):line$=left$(line$,d-1)' : print d,line$,rest$

' 1b: find separators

separators(0)=0
i=0: j=1 : do: i+=1 : let c$=mid$(line$,i,1) 
if isseparator(c$) then separators(j)=i: j+=1
loop until i>l:separators(j)=i

' 1c : split the command to parts

let k=0
for i=0 to j-1 
  let p1=separators(i): let p2=separators(i+1)
  let p$=mid$(line$,p1,1): if p$<>" " andalso p$<>"" then lparts(k).part$=p$ : k+=1 
  let p$=mid$(line$,p1+1,p2-p1-1) : if p$<>" " andalso p$<>"" then lparts(k).part$=p$ : k+=1 
next i

' 1d : find strings

i=0
do
  if lparts(i).part$<>"""" then i+=1  :goto 110
  let q=i: do: let p$=lparts(i+1).part$ : lparts(q).part$=lparts(q).part$+p$: for j=i+1 to k: lparts(j)=lparts(j+1) : next j: k-=1 :  loop until p$="""" orelse i>=k  
  if p$<>"""" then k+=1:i+=1
110 loop until i>=k

' 1e : concatenate strings if "" detected between
 
i=0 : do
 if right$(lparts(i).part$,1)="""" andalso left$(lparts(i+1).part$,1)=""""  then 
   lparts(i).part$=lparts(i).part$+right$(lparts(i+1).part$,len(lparts(i+1).part$)-1)
   for j=i+1 to k: lparts(j)=lparts(j+1): next j :
   i-=1 : k-=1 ' do not move i if concatenated
 endif
 i+=1 : loop until i>=k 



' 1f : now remove parts that are spaces

for i=0 to k-1: lparts(i).part$=trim$(lparts(i).part$): next i
i=0
do 
  if len(lparts(i).part$)=0 then 
    if i=j-1 then k-=1 : exit
    if i<k-1 then for j=i to k-2 : lparts(j)=lparts(j+1): next j: k-=1 :  if i>0 then i-=1 
  endif
 i+=1: loop until i>=k-1


'---------------------------------------


' Pass  2

if len(lparts(0).part$)=0 then goto 101						' empty line, nothing to do


'  2a: find all (), set priority for parts

let p=0: let maxp=0
for i=0 to k-1
  if lparts(i).part$="(" then  p+=1 
  if lparts(i).part$=")" then p-=1
  if p>maxp then maxp=p
  lparts(i).priority=p 
next i

' 2b find part types 0 operators 256 commands 512 data(nums, strings)

for i=0 to k-1
lparts(i).token=isseparator(lparts(i).part$): if lparts(i).token>0 then goto 120
lparts(i).token=isoperator(lparts(i).part$): if lparts(i).token>0 then goto 120
lparts(i).token=isassign(lparts(i).part$) : if lparts(i).token>0 then goto 120
lparts(i).token=iscommand(lparts(i).part$): if lparts(i).token>0 then goto 120

let b1=isnum(lparts(i).part$):let b2=isint(lparts(i).part$):let b3=isdec(lparts(i).part$)
if b1 andalso b2 andalso b3 then lparts(i).token=512 : goto 120 			' pure decimal for line num
if b1 andalso b2 andalso (not b3) then lparts(i).token=513 : goto 120 		' integer
if b1 andalso (not b2) andalso (not b3) then lparts(i).token=514 :goto 120 		' float
if isstring(lparts(i).part$) then lparts(i).token=515 : goto 120			' string
if isname(lparts(i).part$) then lparts(i).token=516 : goto 120
lparts(i).token=-1
120 next i 

'2c try to evaluate expressions

  
if isdec(lparts(0).part$) then print "  This is a program line": goto 101  	'<-- TODO: add a line to a program
if lparts(0).token=516 andalso lparts(1).token>=32  andalso lparts(1).token<64 then print "  this is assigning to a name" : goto 101
if lparts(0).token=516 andalso lparts(1).token=515 then print "  this is calling a function or assigning to an array" : goto 101

' if we are here, this is not a program line to add, so try to execute this


let pos=execute(0) ' print "  this is a command to execute"



for i=0 to k-1: print lparts(i).part$,lparts(i).priority, lparts(i).token: next i : print maxp



101 print "  Ready" : v.write("  ")  
end sub


function isoperator(s as string) as ubyte

select case s
case "+"
  return 1
case "-"
  return 2
case "or"
  return 3
case "xor"
  return 4
case "*"
  return 5
case "/"
  return 6
case "and"
  return 7
case "div"
  return 8
case "mod"
  return 9
case "shl"
  return 10
case "shr" 
  return 11
case "^"
  return 12
case "not"
  return 13
case "@"
  return 14
case "++"        ' not the case as + are separated, todo
  return 15
case "--"        ' the same
  return 16
  
  
end select
return 0 
end function


function isseparator(s as string) as ubyte

select case s
case "+"
  return 1
case "-"
  return 2
case "="
  return 32
case ","
  return 17
case "*"
  return 5
case "/"
  return 6
case ";"
  return 18
case """"
  return 19

case "^"
  return 12

case ")"
  return 20
case "("        ' not the case as + are separated, todo
  return 21
case ":"        ' the same
  return 22
end select
return 0
end function


function isassign(s as string) as ubyte

select case s
case "="
  return 23
case "-="
  return 24
case "+="
  return 25
case "*="
  return 26
case "/="
  return 27
  
end select
return 0  
end function

function isname(s as string) as boolean

' name can be (_a)(1a_.)($%!)

dim i,l,m$ 
 
l=len(s): if l=0 then return false
m$=mid$(s,1,1) : if (m$<"a" orelse m$>"z")  andalso m$<>"_" then return false
if l>2 then 
  for i=2 to l
    m$=mid$(s,i,1) : if (i<l) andalso (m$<"a" orelse m$>"z") andalso (m$<"0" orelse m$>"9") andalso m$<>"_" andalso m$<>"." then return false
    if (i=l) andalso (m$<"a" orelse m$>"z") andalso (m$<"0" orelse m$>"9") andalso m$<>"_" andalso m$<>"$" andalso m$<>"%" andalso m$<>"!" then return false
  next i
endif


return true
end function  


function isnum(s as string) as boolean

dim i,l,m$,ds,es
ds=0: es=0
l=len(s): if l=0 then return false
m$=mid$(s,1,1) : if (m$<"0" orelse m$>"9") andalso m$<>"." andalso m$<>"$" andalso m$<>"%" andalso m$<>"-" then return false
if m$="." then ds=1
if l>1 then 
  for i=2 to l
    m$=mid$(s,i,1) : if (m$<"0" orelse m$>"9") andalso m$<>"_" andalso m$<>"." andalso m$<>"E" andalso m$<>"e" andalso m$<>"-" then return false
    if m$="-" andalso lcase$(mid$(s,i-1,1))<>"e" then return false
    if m$="." then ds+=1: if ds>1 then return false
    if m$="E" orelse m$="e" then es+=1: if es>1 then return false
  next i
endif
return true
end function  
  
function isint(s as string) as boolean

dim i,l,m$,ds,es

l=len(s): if l=0 then return false
m$=mid$(s,1,1) : if (m$<"0" orelse m$>"9") andalso m$<>"$" andalso m$<>"%" andalso m$<>"-" then return false

if l>1 then 
  for i=2 to l
    m$=mid$(s,i,1) : if (m$<"0" orelse m$>"9") andalso m$<>"_"  then return false
  next i
endif
return true
end function  

function isdec(s as string) as boolean

dim i,l,m$,ds,es

l=len(s): if l=0 then return false
for i=1 to l
    m$=mid$(s,i,1) : if (m$<"0" orelse m$>"9") andalso m$<>"_"  then return false
  next i
return true
end function 

function ispar(s as string) as boolean

if s="(" orelse s=")" then return true else return false
end function

function isear(s as string) as boolean

if s="""" then return true else return false
end function



function iscommand(s as string) as ubyte

select case s
case "cls"
  return 32
case "new"
  return 33
case "plot"
  return 34
case "draw"
  return 35
case "print"
  return 36
case "circle"
  return 37
case "fcircle"
  return 38
case "box"
  return 39
case "frame"
  return 40
 
end select
return 0  
end function

function isstring(s as string) as boolean
if left$(s,1)="""" andalso right$(s,1)="""" then return true else return false
end function

const maxcommand=8
dim shared as string command(maxcommand)={_
"cls","new","plot","draw","print","circle","fcircle","box","frame"}

function execute(pos as integer) as integer

let cmd=lparts(pos).token
ct=pos+1
select case cmd
case 32              	'cls
  cls: print "" : return -1
case 33		    	'new
  cls: position 4,1 : print "P2 Retromachine BASIC version 0.01" : return -1
  print " "   ' todo: clear all program structures
case 34			'plot
  do_plot
end select
end function

'------------------------------------------------------------------------------------------------------------
' Expression decoder/evaluator
'------------------------------------------------------------------------------------------------------------

function expr(ct as integer) as expr_result,integer

' On input: ct = current token position
' On output: expression result value and a new ct

dim t1, t2 as expr_result
dim op as integer

t1,ct = muldiv(ct)             			' call higher priority operator check. It will itself call getval/getvar if no multiplies or divides
op = lparts(ct).token				' that idea is from github adamdunkels/ubasic

do while (op = token_plus orelse op = token_minus orelse op = token_and orelse op=token_or)
  ct+=1
  t2,ct = muldiv(ct) 
  select case op
    case token_plus
      t1=do_plus(t1,t2)
    case token_minus
      t1=do_minus(t1,t2)
    case token_and
      t1=do_and(t1,t2)
    case token_or
      t1=do_or(t1,t2)
  end select  
  op = lparts(ct).token
  loop
return t1,ct
end function


function muldiv(ct as integer) as expr_result,integer

dim t1, t2 as expr_result
dim op as integer

t1,ct = getvalue(ct)                       	' get a value to do the operation
op = lparts(ct).token
do while (op = token_mul orelse op = token_div orelse op = token_fdiv orelse op=token_mod orelse op=token_shl orelse op=token_shr orelse op=token_power)
  ct+=1
  t2,ct = getvalue(ct) 
  select case op
    case token_mul
      t1=do_mul(t1,t2)
    case token_div
      t1=do_div(t1,t2)
    case token_fdiv
      t1=do_fdiv(t1,t2)
    case token_mod
      t1=do_mod(t1,t2)
    case token_shl
      t1=do_shl(t1,t2)
    case token_shr
      t1=do_shr(t1,t2)
    case token_power
      t1=do_power(t1,t2)
  end select  
  op = lparts(ct).token
  loop
return t1,ct
end function

function getvalue(ct as integer) as expr_result,integer

dim t1 as expr_result
dim op as integer

op=lparts(ct).token
ct+=1
select case op
  case token_decimal
    t1.result.uresult=val%(lparts(ct).part$): t1.result_type=1 ' todo token_int64
  case token_integer
    t1.result.iresult=val%(lparts(ct).part$): t1.result_type=0  
  case token_float
    t1.result.fresult=val%(lparts(ct).part$): t1.result_type=4  
  case token_string
    t1.result.sresult=lparts(ct).part$: t1.result_type=5  
  case token_name  '' we may got token with var or fun # after evaluation (?) 
    t1,ct=getvar(ct)
  case token_lpar
    t1,ct=expr(ct) ' todo check left par
end select    
return t1,ct
end function


'getvar will go there
'if the next token is not ( then find variable by name, reutrn its value and change token to 1024+var index ?
'if the next token is  ( then find function by name, call do_function,change the token to 2048+fn index ?

function getvar(ct as integer) as expr_result,integer
dim t1 as expr_result
t1.result.iresult=0 : t1.result_type=0 ' mockup
ct+=1
return t1,ct
end function









function do_plus(t1 as expr_result ,t2 as expr_result) as expr_result
'todo
return t1
end function

function do_minus(t1 as expr_result ,t2 as expr_result) as expr_result
'todo
return t1
end function

function do_and(t1 as expr_result ,t2 as expr_result) as expr_result
'todo
return t1
end function

function do_or(t1 as expr_result ,t2 as expr_result) as expr_result
'todo
return t1
end function

function do_mul(t1 as expr_result ,t2 as expr_result) as expr_result
'todo
return t1
end function

function do_div(t1 as expr_result ,t2 as expr_result) as expr_result
'todo
return t1
end function

function do_fdiv(t1 as expr_result ,t2 as expr_result) as expr_result
'todo
return t1
end function

function do_mod(t1 as expr_result ,t2 as expr_result) as expr_result
'todo
return t1
end function

function do_shl(t1 as expr_result ,t2 as expr_result) as expr_result
'todo
return t1
end function

function do_shr(t1 as expr_result ,t2 as expr_result) as expr_result
'todo
return t1
end function

function do_power(t1 as expr_result ,t2 as expr_result) as expr_result
'todo
return t1
end function


'----------------------------------

function do_operator(op as integer, a as expr_result,b as integer, c as expr_result, d as integer) as expr_result
return nil
end function


sub do_plot

dim t1,t2 as expr_result
dim ct as integer
t1,ct=expr(1) : print "ct= "; ct, "result=";t1.result.iresult,"result type= ",t1.result_type
if lparts(ct).token<> token_comma then 
   print"  Error" ' todo: error codes etc
else
  ct+=1
  t2,ct=expr(ct)
endif
end sub

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
