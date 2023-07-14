const _clkfreq = 336956522
const HEAPSIZE=16384
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

const ver$="P2 Retromachine BASIC version 0.06"

' ----------------------- Tokens -----------------------------------------

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
const token_colon=22

const token_assign_eq=23
const token_assign_sub=24
const token_assign_add=25
const token_assign_mul=26
const token_assign_div=27

const token_cls=32
const token_new=33
const token_plot=34
const token_draw=35
const token_print=36
const token_circle=37
const token_fcircle=38
const token_box=39
const token_frame=40
const token_color=41

const token_end=254
const token_space=255
 
const token_decimal=512
const token_integer=513
const token_float=514
const token_string=515
const token_name=516


' ----------------------------- End of token list -------------------------

const result_int=0
const result_uint=1
const result_float=4
const result_string=5
const result_error=6

class part
  dim part$ as string
  dim token as integer
end class

class expr_result
  dim iresult as integer
  dim uresult as ulong
  dim sresult as string
  dim fresult as double
  dim result_type as ulong ' 0 i 1 u 4f 5s 6 error, 2,3 reserved for int64
end class

class integer_variable
  dim name as string
  dim value as integer
end class

class uint_variable
  dim name as string
  dim value as ulong
end class


class float_variable
  dim name as string
  dim value as single
end class

class string_variable
  dim name as string
  dim value as string
end class

' to do : move these out of the heap into the psram...

dim ivariables as integer_variable(1023) ' how to make this better?? let's start from 1k vars available....   
dim uvariables as uint_variable(1023) ' how to make this better?? let's start from 1k vars available....   
dim fvariables as float_variable(1023) ' how to make this better?? let's start from 1k vars available....   
dim svariables as string_variable(1023) ' how to make this better?? let's start from 1k vars available....   

dim ivarnum as integer
dim uvarnum as integer
dim fvarnum as integer
dim svarnum as integer


type parts as part(125) 
dim lparts as parts

dim audiocog,videocog as integer
dim base as ulong
dim mbox as ulong
dim ansibuf(3) as ubyte
dim textscreen (35,127) as ubyte
dim line$ as string
dim testaudio(883) as ushort

dim plot_color,plot_x,plot_y as integer
dim editor_spaces as integer
dim paper,ink as integer
dim ct as integer

'----------------------------------------------------------------------------
'-----------------------------Program start ---------------------------------
'----------------------------------------------------------------------------

startpsram
startvideo
plot_color=154 : plot_x=0: plot_y=0
editor_spaces=2
paper=147: ink=154
ivarnum=0 : uvarnum=0 : fvarnum=0 : svarnum=0
let audiocog,base=paula.start(0,0,0)
waitms(50)
dpoke base+20,16384
kbm.start()
kbm.mouse_set_limits(1023,575)
cls
v.setfontfamily(4) 				' use ST Mono font

position 0,1 : print space$(editor_spaces);ver$
position 0,3 : print space$(editor_spaces);"Ready"
print space$(editor_spaces);
for i=0 to 35: for j=0 to 127: textscreen(i,j)=32: next j: next i
dim key , key2 as ulong
 
dim errors$(255)
init_error_strings
 
 
 
'-------------------------------------------------------------------------------------------------------- 
'-------------------------------------- MAIN LOOP -------------------------------------------------------
'--------------------------------------------------------------------------------------------------------

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
   
 ' To do: arrows and DEL; use textscreen array to implement fullscreen editing
 
  if key4=141 then 
    v.crlf() : print space$(editor_spaces);
    interpret(line$): line$=""
    endif 

  key3=0
  endif

loop

'----------------------------------- this is the end of the main loop ------------------------------------------------------------------

'---------------------------------------------------------------------------------------------------------------------------------------
'----------------------------------- The immediate line interpreter --------------------------------------------------------------------
'---------------------------------------------------------------------------------------------------------------------------------------

sub interpret(line$)

 
dim i,j,k,q
dim result as expr_result
dim etype as integer
dim separators(125)

' a workaround to prevent the gc from disabling prints
'108 close #0
'_gc_collect()
'open SendRecvDevice(@v.putchar, nil, nil) as #0

' ---------------------------------------------------  Pass 1: Split the line to parts, detect and concatenate strings


108 for i=0 to 125: separators(i)=0 :next i
for i=0 to 125: lparts(i).part$="": next i

' 1a : extract the first command, split the line to first command and the rest

line$=trim$(lcase$(line$)):let d$="" : let l=len(line$) 
if l=0 then goto 101
let d=instr(1,line$,":"): if d>0 andalso d<len(line$)  then let rest$=right$(line$,len(line$)-d):line$=left$(line$,d-1) else rest$="" ' : print d,line$,rest$

' 1b: find separators

separators(0)=0
i=0: j=1 : do: i+=1 : let c$=mid$(line$,i,1) 
if isseparator(c$) then separators(j)=i: j+=1 
loop until i>l:separators(j)=i

' 1c : split the command to parts

let k=0
for i=0 to j-1 
  let p1=separators(i): let p2=separators(i+1)' : print p1,p2
  if p1>0 then let p$=mid$(line$,p1,1):  if p$<>" " andalso p$<>"" then lparts(k).part$=p$ : k+=1 
  let p$=mid$(line$,p1+1,p2-p1-1)  : if p$<>" " andalso p$<>"" then lparts(k).part$=p$ : k+=1 
next i

' 1d : find strings

i=0
do
  if lparts(i).part$<>"""" then i+=1 : goto 109
  let q=i: do: let p$=lparts(i+1).part$ : lparts(q).part$=lparts(q).part$+p$: for j=i+1 to k: lparts(j)=lparts(j+1) : next j: k-=1 :  loop until p$="""" orelse i>=k  
  if p$<>"""" then k+=1:i+=1
109 loop until i>=k

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

'-------------------------------------------------------- Pass 2: Tokenize the line

if len(lparts(0).part$)=0 then goto 101				' empty line, nothing to do

' 2a find part types 

for i=0 to k-1
lparts(i).token=isseparator(lparts(i).part$): if lparts(i).token>0 then goto 102
lparts(i).token=isoperator(lparts(i).part$): if lparts(i).token>0 then goto 102
lparts(i).token=isassign(lparts(i).part$) : if lparts(i).token>0 then goto 102
lparts(i).token=iscommand(lparts(i).part$): if lparts(i).token>0 then goto 102

let b1=isnum(lparts(i).part$):let b2=isint(lparts(i).part$):let b3=isdec(lparts(i).part$)
if b1 andalso b2 andalso b3 then lparts(i).token=token_decimal : goto 102 					' pure decimal for line num
if b1 andalso b2 andalso (not b3) then lparts(i).token=token_integer : goto 102 				' integer
if b1 andalso (not b2) andalso (not b3) then lparts(i).token=token_float :goto 102 				' float
if isstring(lparts(i).part$) then lparts(i).token=token_string : lparts(i).part$=mid$(lparts(i).part$,2,len(lparts(i).part$)-2) :goto 102	' string, get id of ""!
if isname(lparts(i).part$) then lparts(i).token=token_name : goto 102						' name
lparts(i).token=-1
102 next i 
lparts(k).token=token_end

'2b determine a type of the line
  
if isdec(lparts(0).part$) then print "  This is a program line": goto 101  								'<-- TODO: add a line to a program
if lparts(0).token=516 andalso lparts(1).token>=token_assign_eq  andalso lparts(1).token<token_assign_div then print "this is assigning to a name" : do_assign : goto 101    '<-- TODO: assign a variable
if lparts(0).token=516 andalso lparts(1).token=515 then print "  this is calling a function or assigning to an array" : goto 101

' if we are here, this is not a program line to add, so try to execute this

let pos=execute(0) ' print "  this is a command to execute"
if rest$<>"" then line$=rest$: goto 108

101 v.writeln("") : v.writeln("  Ready") : v.write("  ")  
end sub

'------------------------------ Helper functions for the tokenizer -------------------------------------------

function isoperator(s as string) as ubyte

select case s
  case "+"   : return token_plus
  case "-"   : return token_minus
  case "or"  : return token_or
  case "xor" : return token_xor
  case "*"   : return token_mul
  case "/"   : return token_fdiv
  case "and" : return token_and
  case "div" : return token_div
  case "mod" : return token_mod
  case "shl" : return token_shl
  case "shr" : return token_shr
  case "^"   : return token_power
  case "not" : return token_not
  case "@"   : return token_at
  case "++"  : return token_inc
  case "--"  : return token_dec
  case else  : return 0 
end select
end function

function isseparator(s as string) as ubyte

select case s
  case "+"   : return token_plus
  case "-"   : return token_minus
  case "="   : return token_assign_eq
  case ","   : return token_comma
  case "*"   : return token_mul
  case "/"   : return token_fdiv
  case ";"   : return token_semicolon
  case """"  : return token_ear
  case "^"   : return token_power
  case ")"   : return token_rpar
  case "("   : return token_lpar
  case ":"   : return token_colon
  case " "   : return token_space
  case else  : return 0
end select
end function

function isassign(s as string) as ubyte

select case s
  case "="   : return token_assign_eq
  case "-="  : return token_assign_sub
  case "+="  : return token_assign_add
  case "*="  : return token_assign_mul
  case "/="  : return token_assign_div
  case else  : return 0  
end select
end function


function iscommand(s as string) as ubyte

select case s
  case "cls"         : return token_cls
  case "new"         : return token_new
  case "plot"        : return token_plot
  case "draw"        : return token_draw
  case "print"       : return token_print
  case "circle"      : return token_circle
  case "fcircle"     : return token_fcircle
  case "box"         : return token_box
  case "frame"       : return token_frame
  case "color"       : return token_color
  case "print"	     : return token_print
  case else          : return 0  
end select
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

function isstring(s as string) as boolean
if left$(s,1)="""" andalso right$(s,1)="""" then return true else return false
end function


'--------------------- The end of parser/tokenizer functions ---------------------------

'---------------------------------------------------------------------------------------
'--------------------- A main immediate execute function -------------------------------
'---------------------------------------------------------------------------------------

function execute(pos as integer) as integer

let cmd=lparts(pos).token
ct=pos+1
select case cmd
case token_cls      : cls : print "" : return -1
case token_new      : cls : position 4,1 : print ver$ : print :return -1
case token_plot     : do_plot  : return -1
case token_draw	    : do_draw  : return -1
case token_fcircle  : do_fcircle : return -1  
case token_color    : do_color :return -1  
case token_print    : do_print :return -1  
end select
end function

'-------------------------------------------------------------------------------------
'------------------ Assigning to a variable ------------------------------------------
'-------------------------------------------------------------------------------------

sub do_assign

dim i,j,ctt as integer
dim t1 as expr_result
ctt=ct: print ct

let varname$=lparts(0).part$: print varname$
let suffix$=right$(varname$,1)
ct=2: t1=expr()
if suffix$="$" andalso t1.result_type<>result_string then printerror(15):goto 501
if suffix$="!" andalso t1.result_type<>result_float andalso t1.result_type<>result_uint andalso t1.result_type<>result_int then printerror(16):goto 501
if suffix$="%" andalso t1.result_type<>result_uint then printerror(17):goto 501
if suffix$<>"$" andalso suffix$<>"!" andalso suffix$<>"%" andalso t1.result_type<>result_uint andalso t1.result_type<>result_int then printerror(18): goto 501
if t1.result_type=result_error then printerror(t1.uresult): goto 501
/'
'select case t1.result_type
'    case result_int:    goto 502
'    case result_uint:   goto 503
'    case result_float:  goto 504
'    case result_string: goto 505
'    end select

502
i=-1: j=-1
if ivarnum>0 then
  for i=0 to ivarnum
    if variables(i).name=varname$ then j=i: exit
  next i
endif
print varname$,t1.result_type,t1.uresult, t1.fresult, t1.sresult

i=-1: j=-1 
if varnum>0 then
  for i=0 to varnum
    if variables(i).name=varname$ then j=i: exit
  next i
endif
if j=-1 then
  variables(varnum).name=varname$
  variables(varnum).vartype=t1.result_type
  select case t1.result_type
    case result_int: variables(varnum).place=new integer: variables(varnum).place(0)=t1.iresult
    case result_uint: variables(varnum).place=new ulong: variables(varnum).place(0)=t1.uresult
    case result_float: variables(varnum).place=new single: variables(varnum).place(0)=t1.fresult
    case result_string: variables(varnum).place=varname$
    end select
    varnum +=1
endif
print varnum,variables(varnum-1).name,variables(varnum-1).place(0),variables(varnum-1).vartype
if variables(varnum-1).vartype=5  then print variables(varnum-1).place
'/
501 end sub




'------------------------------------------------------------------------------------------------------------
' Expression decoder/evaluator
'------------------------------------------------------------------------------------------------------------

function expr( ) as expr_result 

' On input: ct = current token position
' On output: expression result value and a new ct

dim t1, t2 as expr_result
dim op as integer

t1 = muldiv()             			' call higher priority operator check. It will itself call getval/getvar if no multiplies or divides
if t1.result_type=result_error then return t1
op = lparts(ct).token				' that idea is from github adamdunkels/ubasic

do while (op = token_plus orelse op = token_minus orelse op = token_and orelse op=token_or)
  ct+=1
  t2 = muldiv() 
  if t2.result_type=result_error then return t2
  select case op
    case token_plus    : t1=do_plus(t1,t2)
    case token_minus   : t1=do_minus(t1,t2)
    case token_and     : t1=do_and(t1,t2)
    case token_or      : t1=do_or(t1,t2)
  end select  
  op = lparts(ct).token
  loop
return t1 
end function


function muldiv() as expr_result

dim t1, t2 as expr_result
dim op as integer

t1 = getvalue()    
if t1.result_type=result_error then return t1

op = lparts(ct).token
do while (op = token_mul orelse op = token_div orelse op = token_fdiv orelse op=token_mod orelse op=token_shl orelse op=token_shr orelse op=token_power)
  ct+=1
  t2 = getvalue() 
  if t2.result_type=result_error then return t2
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
 
return t1
end function

function getvalue() as expr_result

dim t1 as expr_result
dim op as integer

op=lparts(ct).token

select case op
  case token_decimal
    t1.uresult=val%(lparts(ct).part$): t1.result_type=1 ' todo token_int64
  case token_integer
    t1.iresult=val%(lparts(ct).part$)
    t1.result_type=0  
  case token_float
    t1.fresult=val(lparts(ct).part$): t1.result_type=4  
  case token_string
    t1.sresult=lparts(ct).part$: t1.result_type=5  
'  case token_name  '' we may got token with var or fun # after evaluation (?) 
'    t1=getvar()
  case token_lpar
    ct+=1
    t1=expr()
    if lparts(ct).token<>token_rpar then t1.result_type=result_error: t1.uresult=14 : return t1
end select    
ct+=1
  '  print "Debug from getvalue: "; t1.uresult, t1.iresult, t1.fresult, t1.sresult, "result type: ", t1.result_type

return t1
end function


'getvar will go there
'if the next token is not ( then find variable by name, reutrn its value and change token to 1024+var index ?
'if the next token is  ( then find function by name, call do_function,change the token to 2048+fn index ?

function getvar() as expr_result
dim t1 as expr_result
t1.uresult=0 : t1.result_type=0 ' mockup
ct+=1
return t1
end function

'--------------------------------------------- End of expression evaluator -----------------------------------------------

function getintres(ct as integer) as integer,integer

dim t1 as expr_result 
dim a1 as integer
dim r as integer

t1=expr()
select case t1.result_type
  case 0: a1=t1.iresult : r=0
  case 1: a1=t1.uresult : r=0
  case 4: a1=round(t1.fresult) : r=0
  case 5: a1=val(t1.sresult) :r=0
  case result_error: a1=0: r=t1.uresult
  case else : a1=0 : r=1

end select
return a1,r 

end function

'----------------------------------------------------------------------------------------------------------------------------
'-------------------------------------- Functions that do operators and commands --------------------------------------------
'----------------------------------------------------------------------------------------------------------------------------

function do_plus(t1 as expr_result ,t2 as expr_result) as expr_result



if t1.result_type=result_uint andalso t2.result_type=result_uint then t1.uresult+=t2.uresult :return t1
if t1.result_type=result_uint andalso t2.result_type=result_int then t1.iresult=t1.uresult+t2.iresult: t1.result_type=result_int :return t1
if t1.result_type=result_uint andalso t2.result_type=result_float then t1.fresult=cast(single,t1.uresult)+t2.fresult: t1.result_type=result_float :return t1
if t1.result_type=result_int andalso t2.result_type=result_uint then t1.iresult+=t2.uresult:return t1
if t1.result_type=result_int andalso t2.result_type=result_int then t1.iresult+=t2.iresult:return t1
if t1.result_type=result_int andalso t2.result_type=result_float then t1.fresult=cast(single,t1.iresult)+t2.fresult: t1.result_type=result_float :return t1
if t1.result_type=result_float andalso t2.result_type=result_uint then t1.fresult=t1.fresult+cast(single,t2.uresult) :return t1
if t1.result_type=result_float andalso t2.result_type=result_int then t1.fresult=t1.fresult+cast(single,t2.iresult) :return t1
if t1.result_type=result_float andalso t2.result_type=result_float then t1.fresult+=t2.fresult:return t1
if t1.result_type=result_string andalso t2.result_type<>result_string then t1.uresult=2 :t1.result_type=result_error:return t1
if t2.result_type=result_string andalso t1.result_type<>result_string then t1.uresult=2 :t1.result_type=result_error:return t1
if t1.result_type=result_string andalso t2.result_type=result_string then t1.sresult=t1.sresult+t2.sresult :return t1
t1.uresult=4 : t1.result_type=result_error: return t1
end function

function do_minus(t1 as expr_result ,t2 as expr_result) as expr_result

if t1.result_type=result_uint andalso t2.result_type=result_uint then 
    if t2.uresult<t1.uresult then  t1.uresult-=t2.uresult : return t1 else t1.iresult=t1.uresult-t2.uresult : t1.result_type=result_int : return t1
    endif
if t1.result_type=result_uint andalso t2.result_type=result_int then t1.iresult=t1.uresult-t2.iresult: t1.result_type=result_int :return t1
if t1.result_type=result_uint andalso t2.result_type=result_float then t1.fresult=cast(single,t1.uresult)-t2.fresult: t1.result_type=result_float :return t1
if t1.result_type=result_int andalso t2.result_type=result_uint then t1.iresult-=t2.uresult:return t1
if t1.result_type=result_int andalso t2.result_type=result_int then t1.iresult-=t2.iresult:return t1
if t1.result_type=result_int andalso t2.result_type=result_float then t1.fresult=cast(single,t1.iresult)-t2.fresult: t1.result_type=result_float :return t1
if t1.result_type=result_float andalso t2.result_type=result_uint then t1.fresult=t1.fresult-cast(single,t2.uresult) :return t1
if t1.result_type=result_float andalso t2.result_type=result_int then t1.fresult=t1.fresult-cast(single,t2.iresult) :return t1
if t1.result_type=result_float andalso t2.result_type=result_float then t1.fresult-=t2.fresult:return t1
if t1.result_type=result_string orelse t2.result_type=result_string then t1.uresult=3: t1.result_type=result_error: return t1
t1.uresult=5 : t1.result_type=result_error:return t1
end function

function do_and(t1 as expr_result ,t2 as expr_result) as expr_result

if t1.result_type=result_int then t1.uresult=cast(ulong,t1.iresult) : t1.result_type=result_uint
if t2.result_type=result_int then t2.uresult=cast(ulong,t2.iresult) : t2.result_type=result_uint
if t1.result_type=result_string orelse t2.result_type=result_string orelse t1.result_type=result_float orelse t2.result_type=result_float then t1.uresult=6: t1.result_type=result_error: return t1
t1.uresult=t1.uresult and t2.uresult :return t1
t1.uresult=7 : t1.result_type=result_error:return t1
end function

function do_or(t1 as expr_result ,t2 as expr_result) as expr_result
if t1.result_type=result_int then t1.uresult=cast(ulong,t1.iresult) : t1.result_type=result_uint
if t2.result_type=result_int then t2.uresult=cast(ulong,t2.iresult) : t2.result_type=result_uint
if t1.result_type=result_string orelse t2.result_type=result_string orelse t1.result_type=result_float orelse t2.result_type=result_float then t1.uresult=6: t1.result_type=result_error: return t1
t1.uresult=t1.uresult or t2.uresult :return t1
t1.uresult=7 : t1.result_type=result_error:return t1
end function

function do_mul(t1 as expr_result ,t2 as expr_result) as expr_result

if t1.result_type=result_uint andalso t2.result_type=result_uint then t1.uresult*=t2.uresult :return t1
if t1.result_type=result_uint andalso t2.result_type=result_int then t1.iresult=t1.uresult*t2.iresult: t1.result_type=result_int :return t1
if t1.result_type=result_uint andalso t2.result_type=result_float then t1.fresult=cast(single,t1.uresult)*t2.fresult: t1.result_type=result_float :return t1
if t1.result_type=result_int andalso t2.result_type=result_uint then t1.iresult*=t2.uresult:return t1
if t1.result_type=result_int andalso t2.result_type=result_int then t1.iresult*=t2.iresult:return t1
if t1.result_type=result_int andalso t2.result_type=result_float then t1.fresult=cast(single,t1.iresult)*t2.fresult: t1.result_type=result_float :return t1
if t1.result_type=result_float andalso t2.result_type=result_uint then t1.fresult=t1.fresult*cast(single,t2.uresult) :return t1
if t1.result_type=result_float andalso t2.result_type=result_int then t1.fresult=t1.fresult*cast(single,t2.iresult) :return t1
if t1.result_type=result_float andalso t2.result_type=result_float then t1.fresult*=t2.fresult:return t1
if t1.result_type=result_string orelse t2.result_type=result_string then t1.uresult=8: t1.result_type=result_error: return t1
t1.uresult=9 : t1.result_type=result_error:return t1
end function

function do_div(t1 as expr_result ,t2 as expr_result) as expr_result
if t1.result_type=result_string orelse t2.result_type=result_string then t1.uresult=10: t1.result_type=result_error: return t1
if t1.result_type=result_float then t1.result_type=result_int : t1.iresult=cast(integer,t1.fresult)
if t2.result_type=result_float then t2.result_type=result_int : t2.iresult=cast(integer,t2.fresult)
if t1.result_type=result_uint andalso t2.result_type=result_uint then t1.uresult/=t2.uresult :return t1
if t1.result_type=result_uint andalso t2.result_type=result_int then t1.iresult=t1.uresult/t2.iresult: t1.result_type=result_int :return t1
if t1.result_type=result_int andalso t2.result_type=result_uint then t1.iresult/=t2.uresult :return t1
if t1.result_type=result_int andalso t2.result_type=result_int then t1.iresult=t1.iresult/t2.iresult: return t1
t1.uresult=11 : t1.result_type=result_error:return t1
end function

function do_fdiv(t1 as expr_result ,t2 as expr_result) as expr_result

if t1.result_type=result_string orelse t2.result_type=result_string then t1.uresult=10: t1.result_type=result_error: return t1
if t1.result_type=result_int then t1.result_type=result_float : t1.fresult=cast(single,t1.iresult) 
if t1.result_type=result_uint then t1.result_type=result_float : t1.fresult=cast(single,t1.uresult) 
if t2.result_type=result_int then t2.result_type=result_float : t2.fresult=cast(single,t2.iresult) 
if t2.result_type=result_uint then t2.result_type=result_float : t2.fresult=cast(single,t2.uresult) 
if t1.result_type=result_float andalso t2.result_type=result_float then t1.fresult/=t2.fresult:return t1
t1.uresult=11 : t1.result_type=result_error:return t1
return t1
end function

function do_mod(t1 as expr_result ,t2 as expr_result) as expr_result

if t1.result_type=result_string orelse t2.result_type=result_string then t1.uresult=10: t1.result_type=result_error: return t1
if t1.result_type=result_float then t1.result_type=result_int : t1.iresult=cast(integer,t1.fresult)
if t2.result_type=result_float then t2.result_type=result_int : t2.iresult=cast(integer,t2.fresult)
if t1.result_type=result_uint andalso t2.result_type=result_uint then t1.uresult=t1.uresult mod t2.uresult :return t1
if t1.result_type=result_uint andalso t2.result_type=result_int then t1.iresult=t1.uresult mod t2.iresult: t1.result_type=result_int :return t1
if t1.result_type=result_int andalso t2.result_type=result_uint then t1.iresult=t1.iresult mod t2.uresult :return t1
if t1.result_type=result_int andalso t2.result_type=result_int then t1.iresult=t1.iresult mod t2.iresult: return t1
t1.uresult=11 : t1.result_type=result_error:return t1
end function

function do_shl(t1 as expr_result ,t2 as expr_result) as expr_result

if t1.result_type=result_int then t1.uresult=cast(ulong,t1.iresult) : t1.result_type=result_uint
if t2.result_type=result_int then t2.uresult=cast(ulong,t2.iresult) : t2.result_type=result_uint
if t1.result_type=result_string orelse t2.result_type=result_string orelse t1.result_type=result_float orelse t2.result_type=result_float then t1.uresult=6: t1.result_type=result_error: return t1
t1.uresult=t1.uresult shl t2.uresult :return t1
t1.uresult=7 : t1.result_type=result_error:return t1
end function

function do_shr(t1 as expr_result ,t2 as expr_result) as expr_result

if t1.result_type=result_int then t1.uresult=cast(ulong,t1.iresult) : t1.result_type=result_uint
if t2.result_type=result_int then t2.uresult=cast(ulong,t2.iresult) : t2.result_type=result_uint
if t1.result_type=result_string orelse t2.result_type=result_string orelse t1.result_type=result_float orelse t2.result_type=result_float then t1.uresult=6: t1.result_type=result_error: return t1
t1.uresult=t1.uresult shr t2.uresult :return t1
t1.uresult=7 : t1.result_type=result_error:return t1
end function

function do_power(t1 as expr_result ,t2 as expr_result) as expr_result
if t1.result_type=result_string orelse t2.result_type=result_string then t1.uresult=12: t1.result_type=result_error: return t1
if t1.result_type=result_int then t1.result_type=result_float : t1.fresult=cast(single,t1.iresult) 
if t1.result_type=result_uint then t1.result_type=result_float : t1.fresult=cast(single,t1.uresult) 
if t2.result_type=result_int then t2.result_type=result_float : t2.fresult=cast(single,t2.iresult) 
if t2.result_type=result_uint then t2.result_type=result_float : t2.fresult=cast(single,t2.uresult) 
if t1.result_type=result_float andalso t2.result_type=result_float then t1.fresult=t1.fresult^t2.fresult:return t1
t1.uresult=13 : t1.result_type=result_error:return t1
return t1
end function


'----------------------------------

function do_operator(op as integer, a as expr_result,b as integer, c as expr_result, d as integer) as expr_result
return nil
end function


sub do_plot

dim t1,t2 as expr_result
dim a1,a2 as integer
ct=1 ' ct should be a parameter
t1=expr() ': print "ct= "; ct, "result=";t1.uresult,"result type= ",t1.result_type
if lparts(ct).token<> token_comma then 
   print"  Error" ' todo: error codes etc
else
  ct+=1
  t2=expr()
endif
select case t1.result_type
  case 0: a1=t1.iresult
  case 1: a1=t1.uresult
  case 4: a1=round(t1.fresult)
  case 5: a1=val(t1.sresult)
end select
select case t2.result_type
  case 0: a2=t2.iresult
  case 1: a2=t2.uresult
  case 4: a2=round(t2.fresult)
  case 5: a2=val(t2.sresult)
end select 
plot_x=a1
plot_y=a2
v.putpixel(a1,a2,plot_color) 
end sub

sub do_color

dim a1,r as integer
a1,r=getintres(ct)
if r<>0 then printerror(r) else plot_color=a1 
end sub

sub do_print ' todo reconfigurable editor start position

dim t1 as expr_result
dim r as integer
ct=1
if lparts(ct).token=token_end then print: print space$(editor_spaces) : goto 811
do
t1=expr()
  if t1.result_type=result_error then printerror(t1.uresult): goto 811
if lparts(ct).token=token_comma then
  if t1.result_type=result_int then print t1.iresult,
  if t1.result_type=result_uint then print t1.uresult,
  if t1.result_type=result_float then print t1.fresult,
  if t1.result_type=result_string then print t1.sresult,
endif  
if lparts(ct).token=token_semicolon then 
  if t1.result_type=result_int then print t1.iresult;
  if t1.result_type=result_uint then print t1.uresult;
  if t1.result_type=result_float then print t1.fresult;
  if t1.result_type=result_string then print t1.sresult;
endif
if lparts(ct).token=token_end then 
  if t1.result_type=result_int then print t1.iresult
  if t1.result_type=result_uint then print t1.uresult
  if t1.result_type=result_float then print t1.fresult
  if t1.result_type=result_string then print t1.sresult
endif 
if lparts(ct).token=token_end then goto 811
if lparts(ct).token <>token_comma andalso lparts(ct).token <>token_semicolon andalso lparts(ct).token <>token_end then print "Error: ";lparts(ct).part$ :goto 811 
ct+=1

loop until lparts(ct).token=token_end


811 end sub


sub do_draw

dim t1,t2 as expr_result
dim a1,a2 as integer
ct=1 ' ct should be a parameter
t1=expr()' : print "ct= "; ct, "result=";t1.uresult,"result type= ",t1.result_type
if lparts(ct).token<> token_comma then 
   print"  Error" ' todo: error codes etc
else
  ct+=1
  t2=expr() ': print "ct= "; ct, "result=";t1.uresult,"result type= ",t1.result_type
endif
select case t1.result_type
  case 0: a1=t1.iresult
  case 1: a1=t1.uresult
  case 4: a1=round(t1.fresult)
  case 5: a1=val(t1.sresult)
end select
select case t2.result_type
  case 0: a2=t2.iresult
  case 1: a2=t2.uresult
  case 4: a2=round(t2.fresult)
  case 5: a2=val(t2.sresult)
end select 

v.draw(plot_x,plot_y,a1,a2,plot_color) 
plot_x=a1
plot_y=a2
end sub


sub do_fcircle

dim t1,t2,t3 as expr_result
dim a1,a2,a3 as integer
ct=1 ' ct should be a parameter
t1=expr()' : print "ct= "; ct, "result=";t1.uresult,"result type= ",t1.result_type
if lparts(ct).token<> token_comma then goto 800
ct+=1
t2=expr() ': print "ct= "; ct, "result=";t1.uresult,"result type= ",t1.result_type
if lparts(ct).token<> token_comma then goto 800
ct+=1
t3=expr()



select case t1.result_type
  case 0: a1=t1.iresult
  case 1: a1=t1.uresult
  case 4: a1=round(t1.fresult)
  case 5: a1=val(t1.sresult)
end select
select case t2.result_type
  case 0: a2=t2.iresult
  case 1: a2=t2.uresult
  case 4: a2=round(t2.fresult)
  case 5: a2=val(t2.sresult)
end select 
select case t3.result_type
  case 0: a3=t3.iresult
  case 1: a3=t3.uresult
  case 4: a3=round(t3.fresult)
  case 5: a3=val(t3.sresult)
end select 
v.fcircle(a1,a2,a3,plot_color) 
 
800 end sub



'---------------- Hepler functions

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

'dim shared as integer cargs(maxcommand)={
 '0,0,2,2,-1,3,3,4,4} ' these are argument number for commands. If -1, it is not defined, if from..to , low byte is to, high byte is from



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

sub init_error_strings
errors$(0)=""
errors$(1)="Expected number, got something else."
errors$(2)="Cannot add a number to a string."
errors$(3)="Cannot substract strings."
errors$(4)="Unknown error while adding."
errors$(5)="Unknown error while substracting."
errors$(6)="Cannot do logic operation on string or float."
errors$(7)="Unknown error while doing logic operation."
errors$(8)="Cannot multiply strings."
errors$(9)="Unknown error while multiplying."
errors$(10)="Cannot divide strings."
errors$(11)="Unknown error while dividing."
errors$(12)="Cannot compute a power of a string."
errors$(13)="Unknown error while computing a power."
errors$(14)="Right parenthesis expected."
errors$(15)="Expected string."
errors$(16)="Expected float."
errors$(17)="Expected unsigned integer."
errors$(18)="Expected integer."
end sub
        
sub printerror(err as integer)

v.write("Error " ): v.write(v.inttostr(err)) : v.write(": ")  : v.writeln(errors$(err))
end sub
 
 
asm shared
atari_spl file "atari.spl"
atari2_spl file "atari2.spl" '1758
end asm
