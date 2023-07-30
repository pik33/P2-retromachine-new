const _clkfreq = 336956522
const HEAPSIZE=32768
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

''-----------------------------------------------------------------------------------------
''---------------------------------- Constants --------------------------------------------
''-----------------------------------------------------------------------------------------

const ver$="P2 Retromachine BASIC version 0.13"
const ver=13
'' ------------------------------- Keyboard constants

const   key_enter=141    
const   key_escape=155    
const   key_backspace=136 
const   key_tab=137       
const   key_f1=186        
const   key_f2=187        
const   key_f3=188        
const   key_f4=189        
const   key_f5=190        
const   key_f6=191       
const   key_f7=192       
const   key_f8=193     
const   key_f9=194      
const   key_f10=195     
const   key_f11=196       
const   key_f12=197        
const   key_rightarrow=206 
const   key_leftarrow=207  
const   key_downarrow=208  
const   key_uparrow=209    

' ---------------------------------- Tokens 

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
const fun_getivar=17  ' at runtime, reuse non-function tokens with functions that have no compile time tokens
const fun_getuvar=18
const fun_getfvar=19
const fun_getsvar=20
const fun_negative=21
const fun_converttoint=22
const fun_assign_i=23
const fun_assign_u=24
const fun_assign_f=25
const fun_assign_s=26
const token_eq=27
const fun_pushi=28
const fun_pushu=29
const fun_pushf=30
const fun_pushs=31
const print_mod_empty=32
const print_mod_comma=33
const print_mod_semicolon=34
const token_linenum_major=35
const token_linenum_minor=36
const token_nextline_ptr=37
const token_cls=64
const token_new=65
const token_plot=66
const token_draw=67
const token_print=68
const token_circle=69
const token_fcircle=70
const token_box=71
const token_frame=72
const token_color=73
const token_for=74
const token_next=75
const token_list=76
const token_run=77
const token_fast_goto=78
const token_slow_goto=79
const token_csave=80
const token_save=81
const token_load=82

const token_error=255
const token_end=510
const token_space=511
const token_decimal=512
const token_integer=513
const token_float=514
const token_string=515
const token_name=516
 

' ----------------------------- Expression results/variable types 

const result_int=fun_pushi      ' variable type encodes its own push function in the compiled line
const result_uint=fun_pushu
const result_float=fun_pushf
const result_string=fun_pushs
const result_error=token_error

' -----------------------------max number of variables and stack depth
const maxvars=1023       
const maxgoto=1023       
const maxstack=128

''-----------------------------------------------------------------------------------------
''---------------------------------- Classes and types ------------------------------------
''-----------------------------------------------------------------------------------------

class part                       ' source code line part
  dim part$ as string
  dim token as integer
end class

union aresult			' one long for all result types (until I implement double and int64)
  dim iresult as integer
  dim uresult as ulong
  dim sresult as string
  dim fresult as double
  dim ulresult as ulongint  ' make an 8 byte placeholder for in64 and double
end union
  
class expr_result		' general variable, not only expression result :) 
  dim result as aresult
  dim result_type as ulong  
end class

class integer_variable		' integer variable class for a variable table
  dim name as string
  dim value as integer
end class

class uint_variable		' unsigned integer variable class for a variable table
  dim name as string
  dim value as ulong
end class
 
class float_variable            ' single precision float variable class for a variable table
  dim name as string
  dim value as single
end class

class string_variable           ' string variable class for a variable table
  dim name as string
  dim value as string
end class

class goto_entry
  dim source as ulong
  dim dest as ulong
end class

type parts as part(125)         ' parts to split the line into, line has 125 chars max
type asub as sub()		' sub type to make a sub table

''-----------------------------------------------------------------------------------------
''---------------------------------- Global variables ------------------------------------
''-----------------------------------------------------------------------------------------

dim ivariables as integer_variable(maxvars) ' int vars table 
dim uvariables as uint_variable(maxvars)    ' uint vars table   
dim fvariables as float_variable(maxvars)   ' single vars table   
dim svariables as string_variable(maxvars)  ' string vars table   

dim ivarnum as integer			
dim uvarnum as integer
dim fvarnum as integer
dim svarnum as integer
 
dim lparts as parts

dim audiocog,videocog as integer
dim base as ulong
dim mbox as ulong
dim ansibuf(3) as ubyte
dim line$ as string

dim plot_color,plot_x,plot_y as integer
dim editor_spaces as integer
dim paper,ink as integer
dim ct as integer
dim progend as integer
dim stack(maxstack) as expr_result
dim stackpointer as integer
dim programptr as integer

dim commands(255) as asub 'this is a function table
dim tokennum as integer
dim compiledslot as integer
dim test as expr_result 
dim key , key2 as ulong
dim errors$(255)

dim compiledline(127) as expr_result
declare ucompiledline alias compiledline as ulong(383)
dim lineptr as integer
dim lineptr_e as integer
dim programstart as ulong
dim lastline as ulong
dim lastlineptr as ulong
dim gototable(maxgoto) as goto_entry
dim gotoptr as integer
dim currentdir$ as string

dim sample(255) as ubyte ' for csave
dim block(1023) as ubyte ' for csave
dim blockptr as ulong

'dim varname$ as string ' for compile_assign
'----------------------------------------------------------------------------
'-----------------------------Program start ---------------------------------
'----------------------------------------------------------------------------

startpsram
startvideo
plot_color=154 : plot_x=0: plot_y=0
editor_spaces=2
paper=147: ink=154
compiledslot=sizeof(test)

init_commands
init_error_strings
do_new

audiocog,base=paula.start(0,0,0)
waitms(50)
dpoke base+20,16384
kbm.start()
kbm.mouse_set_limits(1023,575)
cls
v.setfontfamily(4) 				' use ST Mono font
v.setleadingspaces(2)
mount "/sd", _vfs_open_sdcard()
chdir "/sd"
currentdir$="/sd/"

position 2*editor_spaces,1 : print ver$
print v.buf_ptr;" BASIC bytes free"
position 2*editor_spaces,4 : print "Ready"


'-------------------------------------------------------------------------------------------------------- 
'-------------------------------------- MAIN LOOP -------------------------------------------------------
'--------------------------------------------------------------------------------------------------------

do
waitvbl

'' Do key repeat

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
 
  if key4>0 andalso key4<127 andalso v.cursor_x<254 then line$+=chr$(key4): v.putchar(key4)
  if key4>0 andalso key4<127 andalso v.cursor_x=254 then paula.play(0,@atari2_spl,44100,16384,0,1758): waitms(300): paula.stop(0) 'end of line reached
 
  'tab
  if (key3 and 255) = 43 andalso v.cursor_x>=240 then paula.play(0,@atari2_spl,44100,16384,0,1758): waitms(300): paula.stop(0)
  if (key3 and 255) = 43 andalso v.cursor_x<240 then let x=(v.cursor_x mod 16)/2: for i=x to 7: line$+=" " :  v.write (" ") : next i  
 
  'backspace
  if (key3 and 255) = 42 then 
      if v.cursor_x>4 then 
        line$=left$(line$,len(line$)-1): position v.cursor_x-2,v.cursor_y: v.putchar(32) : position v.cursor_x-2,v.cursor_y
      else
         line$="" : v.cursor_x=4
      endif   
   endif   
   
 ' To do: arrows and DEL; use textscreen array to implement fullscreen editing
 
  if key4=key_enter then 
    v.crlf() 
      interpret: line$="" :let t1=getct()-t1 
    endif 

  key3=0 :rpt=0: rptcnt=0
  endif

loop

'----------------------------------- this is the end of the main loop ------------------------------------------------------------------

'---------------------------------------------------------------------------------------------------------------------------------------
'----------------------------------- The line interpreter/tokenizer --------------------------------------------------------------------
'---------------------------------------------------------------------------------------------------------------------------------------

sub interpret

 
dim i,j,k,q
dim result as expr_result
dim etype as integer
dim separators(125)


' ---------------------------------------------------  Pass 1: Split the line to parts, detect and concatenate strings


108 for i=0 to 125: separators(i)=0 :next i
for i=0 to 125: lparts(i).part$="": next i
lineptr=0 


' 1a : extract the first command, split the line to first command and the rest

line$=trim$(line$):let d$="" : let l=len(line$) ' TODO BUG!!!!!!! strings has to be left intact !!!!! Don't lcase$, don't reduce spaces before extracting strings
if l=0 then goto 101
let d=instr(1,line$,":"): if d>0 andalso d<len(line$)  then let rest$=right$(line$,len(line$)-d):line$=left$(line$,d-1) else rest$="" 


' 1b: find separators

separators(0)=0
i=0: j=1 : do: i+=1 : let c$=mid$(line$,i,1) 
if isseparator(c$) then separators(j)=i: j+=1 
loop until i>l:separators(j)=i

' 1c : split the command to parts

let k=0
for i=0 to j-1 
  let p1=separators(i): let p2=separators(i+1)' : print p1,p2
  if p1>0 then let p$=mid$(line$,p1,1):  if   p$<>"" then lparts(k).part$=p$ : k+=1 
  let p$=mid$(line$,p1+1,p2-p1-1)  : if   p$<>"" then lparts(k).part$=p$ : k+=1 
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
    if i=k-1 then k-=1 : exit
    if i<k-1 then for j=i to k-2 : lparts(j)=lparts(j+1): next j: k-=1 :  if i>0 then i-=1 
  endif
 i+=1: loop until i>=k-1



' 1g: lowercase all that is not a string

for j=0 to k-1
  if left$(lparts(j).part$,1)<>"""" orelse right$(lparts(j).part$,1)<>"""" then lparts(j).part$=lcase$(lparts(j).part$) 
next j

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
lparts(k).token=token_end : tokennum=k

''for i=0 to k:print lparts(i).part$,lparts(i).token:next i

'2b determine a type of the line
 
if isdec(lparts(0).part$) andalso lparts(2).token<>token_eq then   compile(val%(lparts(0).part$)) : goto 104  								'<-- TODO: add a line to a program
if isdec(lparts(0).part$) andalso lparts(2).token=token_eq then compile_assign(val%(lparts(0).part$)) : goto 104  								'<-- TODO: add a line to a program
if lparts(0).token=token_name andalso lparts(1).token=token_eq then compile_assign(0) : goto 103    					' assign a variable
if lparts(0).token=token_name andalso lparts(1).token=token_rpar then print " User functions and arrays not yet implemented" : goto 101

' if we are here, this is not a program line to add, so try to execute this

compile(0) : '' execute(0) ' print "  this is a command to execute"  ''' param=line to compile
103 ' for i=0 to lineptr: print compiledline(i).result_type;" ";compiledline(i).result.uresult, : next i
execute_line()

if rest$<>"" then line$=rest$: goto 108

101 v.writeln("") : v.writeln("Ready") 
104 end sub


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
  case "="   : return token_eq ' the compiler then will determine what type is to assign
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
  case "="   : return token_eq
'  case "-="  : return token_assign_sub
'  case "+="  : return token_assign_add
'  case "*="  : return token_assign_mul
'  case "/="  : return token_assign_div
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
  case "list"	     : return token_list
  case "run"	     : return token_run
  case "goto"	     : return token_fast_goto
  case "csave"	     : return token_csave
  case "save"	     : return token_save
  case "load"	     : return token_load
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

function isstring(s as string) as boolean
if left$(s,1)="""" andalso right$(s,1)="""" then return true else return false
end function

'--------------------- The end of interpreter/tokenizer functions ----------------------------------------------------------------------
'---------------------------------------------------------------------------------------------------------------------------------------

'---------------------------------------------------------------------------------------------------------------------------------------
'----------------------Reverse Polish notation precompiler -----------------------------------------------------------------------------
'---------------------------------------------------------------------------------------------------------------------------------------

'----- delete a line from a program

function deleteline(aline as ulong) as integer

dim lineptr2,searchptr as ulong

searchptr=programstart
dim header as ulong(5) 

do
  psram.read1(varptr(header),searchptr,24)
  lineptr2=searchptr
  searchptr=header(5)
loop until header(0)>=aline orelse header(5)=$7FFF_FFFF  ' we have a line that number is >= new line


if header(0)<>aline then return -1

if header(5)=$7FFF_FFFF andalso header(4)=$FFFF_FFFF then ' this is one and only line in the program
  programstart=0 : programptr=0 : lastline=0 : lastlineptr=-1  
  header(0)=$FFFF_FFFF : header(1)=0 : header(2)=0: header(3)=0: header(4)=0: header(5)=0 
  psram.write(varptr(header),0,24)
  return 0
endif

if header(5)=$7FFF_FFFF andalso header(4)<>$FFFF_FFFF then ' this is the last, and not first, line of the program
  pslpoke(header(4)+20,$7FFF_FFFF) ' unlink the previous line
  lastlineptr=header(4)            ' keep last line pointer to avoid searching while sequentially adding a new line
  lastline=pslpeek(header(4))
  return 0
endif   

if header(5)<>$7FFF_FFFF andalso header(4)=$FFFF_FFFF then ' this is the first line, but not the last
   print "deleted first line"
  pslpoke(header(5)+16,$FFFF_FFFF) 
  programstart=header(5) ' adjust the program start to point on the first new line
  return 0
endif

if header(5)<>$7FFF_FFFF andalso header(4)<>$FFFF_FFFF then ' the line is not first and not last
   pslpoke(header(5)+16,header(4))  
   pslpoke(header(4)+20, header(5))
   return 0
endif   
   
end function

sub save_line

dim llength,llength2,llength3 as ulong

llength=compiledslot*(lineptr+1)
llength2=len (line$): if llength2 mod 4 <>0 then llength2=4*((llength2/4)+1)
llength3=llength+llength2
ucompiledline(2)=programptr+llength
ucompiledline(3)=llength2 
   
psram.write(varptr(compiledline),programptr,llength)
psram.write(lpeek(varptr(line$)),programptr+llength,llength2)
programptr+=llength3

end sub


function insertline(aline as ulong) as integer
   
dim searchptr,lineptr2 as ulong

searchptr=programstart
dim header as ulong(5) 

do
  psram.read1(varptr(header),searchptr,24)
  lineptr2=searchptr
  searchptr=header(5)
loop until header(0)>=aline orelse header(5)=$7FFF_FFFF  ' we have a line that number is >= new line

if header(0)=aline then return -1 ' delete it first
if header(0)<aline then return -2 ' end of program reached

if  header(4)=$FFFF_FFFF then ' this is one first line in the program so the inserted line will be new first
  programstart=programptr
  pslpoke(lineptr2+16,programptr)
  ucompiledline(4)=$FFFF_FFFF
  ucompiledline(5)=lineptr2
  save_line
  return 0
endif

if header(4)<>$FFFF_FFFF then ' this is not first line of the program. It doesn't matter if it is last as we will insert it before
  ucompiledline(4)=header(4)
  ucompiledline(5)=lineptr2
  pslpoke(lineptr2+16,programptr)
  pslpoke(header(4)+20,programptr)
  save_line
  return 0
endif  
end function

sub add_line_at_end(aline) 

lastline=aline: ucompiledline(4)=lastlineptr : pslpoke(lastlineptr+20,programptr) : lastlineptr=programptr : ucompiledline(5)=$7FFF_FFFF 
if programptr=0 then ucompiledline(4)=$FFFFFFFF ' that is the first line
save_line
pslpoke(programptr,$FFFFFFFF) ' write end flag ' is it eeded at all here? 
end sub

sub compile_immediate(linetype as ulong)

dim cmd,err as ulong
dim t3 as expr_result

cmd=0
if linetype=0 then cmd=lparts(0).token : ct=1 : lineptr=0 
if linetype=1 then cmd=lparts(1).token : ct=2 : lineptr=2


select case cmd
  case token_cls      : compile_nothing()   'no params, do nothing, only add a command to the line, but case needs something to do after 
  case token_new      : compile_nothing()   
  case token_list     : compile_nothing()   
  case token_run      : compile_nothing()   
  case token_plot     : err=compile_int_fun_2p()   
  case token_draw     : err=compile_int_fun_2p()   
  case token_fcircle  : err=compile_int_fun_3p()  
  case token_color    : err=compile_int_fun_1p()  
  case token_print    : err=compile_print()  : goto 450
  case token_fast_goto     : if linetype=1 then compile_goto()  : goto 450 else printerror(25) : goto 450
  case token_csave    : compile_fun_1p()  
  case token_save    : compile_fun_1p()  'todo compile_str_fun_1p
  case token_load    : compile_fun_1p()  'todo compile_str_fun_1p

  case else	      : compile_unknown() : goto 450

end select
t3.result_type=cmd : compiledline(lineptr)=t3:  lineptr+=1
450 compiledline(lineptr).result_type=token_end 
end sub


sub compile_immediate_assign(linetype as ulong)

dim t1 as expr_result
dim i,j as integer
dim  suffix2$ as string
dim varname2$ as string

t1.result_type=result_error : t1.result.uresult=0
i=-1: j=-1

if linetype=0 then varname2$=lparts(0).part$ :  ct=2  : lineptr=0 
if linetype=1 then varname2$=lparts(1).part$ :  ct=3  : lineptr=2
'print "Called compile immediate assign with linetype",linetype
suffix2$=right$(varname2$,1)
expr()


if suffix2$="$"  then
  if svarnum>0 then
    for i=0 to svarnum-1
      if svariables(i).name=varname2$ then j=i : exit
    next i
  endif
  if  j=-1 andalso svarnum<maxvars then   
    svariables(svarnum).name=varname2$
    j=svarnum
    svarnum+=1
  endif
  t1.result.uresult=j: t1.result_type=fun_assign_s  

else if suffix2$="%" then
  if uvarnum>0 then
    for i=0 to uvarnum-1
      if uvariables(i).name=varname2$ then j=i : exit
    next i
  endif
  if  j=-1 andalso uvarnum<maxvars then   
    uvariables(uvarnum).name=varname2$
    j=uvarnum
    uvarnum+=1
  endif
  t1.result.uresult=j: t1.result_type=fun_assign_u  
   
else if suffix2$="!" then
  if fvarnum>0 then
    for i=0 to fvarnum-1
      if fvariables(i).name=varname2$ then j=i : exit
    next i
  endif
  if  j=-1 andalso fvarnum<maxvars then   
    fvariables(fvarnum).name=varname2$
    j=fvarnum
    fvarnum+=1
  endif
  t1.result.uresult=j: t1.result_type=fun_assign_f  

else '  if suffix$<>"$" andalso suffix$<>"!" andalso suffix$<>"%"  then
  if ivarnum>0 then
    for i=0 to ivarnum-1
      if ivariables(i).name=varname2$ then j=i : exit
    next i
  endif
  if  j=-1 andalso ivarnum<maxvars then   
    ivariables(ivarnum).name=varname2$
    j=ivarnum
    ivarnum+=1  
  endif
  t1.result.uresult=j: t1.result_type=fun_assign_i  
endif  

compiledline(lineptr)=t1:  lineptr+=1 
compiledline(lineptr).result_type=token_end 



end sub

' ------------------ compile the line that is calling a command 

sub compile (alinemajor as ulong, alineminor=0 as ulong)

'line header: num major, num minor,list start, list length, prev, next. That implements 2-way list of program lines 
' num_minor bit 31: the line is goto target. If deleted, a proper record(s) has to be added to goto list
 
if alinemajor=0 then compile_immediate(0) : return  

ucompiledline(0)=alinemajor
ucompiledline(1)=alineminor
compile_immediate(1)

if alinemajor >lastline then 
  add_line_at_end(alinemajor)
else
  deleteline(alinemajor)  
  if alinemajor>lastline then add_line_at_end(alinemajor) else insertline(alinemajor) ' yes I know that's not optimal    
endif 
end sub

' ------------------ compile the line that is assigning to a variable

sub compile_assign (alinemajor as ulong, alineminor=0 as ulong)  


if alinemajor=0 then compile_immediate_assign(0) : return  

ucompiledline(0)=alinemajor
ucompiledline(1)=alineminor
compile_immediate_assign(1)
if alinemajor >lastline then 
  add_line_at_end(alinemajor)
else
  deleteline(alinemajor)  
  if alinemajor>lastline then add_line_at_end(alinemajor) else insertline(alinemajor)   
endif 

end sub

' --------------- Helper compile functions 

sub compile_nothing
end sub

sub compile_unknown() 

dim t1 as expr_result 
t1.result_type=token_error : t1.result.uresult=23
compiledline(lineptr)=t1: lineptr+=1 
end sub

sub compile_converttoint() 

dim t1 as expr_result 
t1.result.uresult=0
expr()
t1.result_type=fun_converttoint
compiledline(lineptr)=t1: lineptr+=1 
end sub

function compile_int_fun_1p() as ulong
 
compile_converttoint()  
return 0
end function

function compile_int_fun_2p() as ulong
 
compile_converttoint()  
if lparts(ct).token<> token_comma then return 21 else ct+=1 ' todo error
compile_converttoint() 
return 0
end function

function compile_int_fun_3p() as ulong
 
compile_converttoint() 
if lparts(ct).token<> token_comma then return 21 else ct+=1 ' todo error
compile_converttoint() 
if lparts(ct).token<> token_comma then return 21 else ct+=1 ' todo error
compile_converttoint() 
return 0
end function

function compile_print() as ulong ' todo reconfigurable editor start position

dim t1 as expr_result
t1.result.uresult=0 : t1.result_type=result_uint
if lparts(ct).token=token_end then t1.result_type=print_mod_empty: compiledline(lineptr)=t1:  lineptr+=1 : t1.result_type=token_print : compiledline(lineptr)=t1:  lineptr+=1 :return 0 	'print without parameters
do
  expr()
  if lparts(ct).token=token_comma then t1.result_type=print_mod_comma : compiledline(lineptr)=t1:  lineptr+=1 : t1.result_type=token_print : compiledline(lineptr)=t1:  lineptr+=1
  if lparts(ct).token=token_semicolon then  t1.result_type=print_mod_semicolon : compiledline(lineptr)=t1:  lineptr+=1 : t1.result_type=token_print : compiledline(lineptr)=t1:  lineptr+=1
  if lparts(ct).token=token_end then t1.result_type=token_print : compiledline(lineptr)=t1:  lineptr+=1
  if lparts(ct).token <>token_comma andalso lparts(ct).token <>token_semicolon andalso lparts(ct).token <>token_end then return 22
  ct+=1  
loop until lparts(ct).token=token_end orelse ct>=tokennum
return 0
end function

function compile_goto( ) as ulong

if lparts(ct).token=token_decimal andalso lparts(ct+1).token=token_end then 

   print "We have a fast goto from ";ucompiledline(0);" to ";val%(lparts(ct+1).part$) 
   
' we can compile fast goto
'fast goto: find a line in the program that has a number to go, compile the do_fast_goto with the pointer
' also add a goto to a goto scrachh table
' if a new line is compiled, check the goto table. If the line is in the goto list, adjust the pointer in the goto source line
else
   print "We have a slow goto "
endif
' if not, there is a slow goto. Call converttoint to get an int value from expression, then do_slow_goto
' Do_slow_goto searches a line pointer list to find the linenum and pointer, then do the goto  
return 0
end function

function compile_fun_1p() as ulong

expr()
return 0
end function


'---------------------------------------------------------------------------------------------------------------------------------------
'------------------------------ End of the precompiler  --------------------------------------------------------------------------------
'---------------------------------------------------------------------------------------------------------------------------------------


'---------------------------------------------------------------------------------------------------------------------------------------
'---------------------------- Compile time expression decoder/evaluator ----------------------------------------------------------------
'---------------------------------------------------------------------------------------------------------------------------------------

sub expr() 

' On input: ct = current token position
' On output: expression result value and a new ct

dim t3 as expr_result
dim op as integer
t3.result.uresult=0
muldiv()             			' call higher priority operator check. It will itself call getval/getvar if no multiplies or divides
op = lparts(ct).token				' that idea is from github adamdunkels/ubasic
do while (op = token_plus orelse op = token_minus orelse op = token_and orelse op=token_or)
  ct+=1
  muldiv() 
  t3.result_type=op: compiledline(lineptr)=t3: lineptr+=1
  op = lparts(ct).token
  loop
end sub

sub muldiv()

dim t3  as expr_result 
dim op as integer
t3.result.uresult=0
getvalue()    
op = lparts(ct).token
do while (op = token_mul orelse op = token_div orelse op = token_fdiv orelse op=token_mod orelse op=token_shl orelse op=token_shr orelse op=token_power)
  ct+=1
  getvalue() 
  t3.result_type=op: compiledline(lineptr)=t3: lineptr+=1
  op = lparts(ct).token
  loop
end sub

sub getvalue() 

dim t1 as expr_result
dim op,m as integer
m=1
t1.result.uresult=0: t1.result_type=result_uint
op=lparts(ct).token
if op=token_minus then m=-1: ct+=1 : op=lparts(ct).token
select case op
  
  case token_decimal
    if m=1 then t1.result.uresult=m*val%(lparts(ct).part$): t1.result_type=result_uint ' todo token_int64
    if m=-1 then t1.result.iresult=m*val%(lparts(ct).part$): t1.result_type=result_int ' todo token_int64
    compiledline(lineptr)=t1: lineptr+=1 :ct+=1
  case token_integer
    t1.result.iresult=m*val%(lparts(ct).part$)
    t1.result_type=result_int  
    compiledline(lineptr)=t1: lineptr+=1 :ct+=1
  case token_float
    if m=1 then t1.result.fresult=1.0*val(lparts(ct).part$): t1.result_type=result_float  
    if m=-1 then t1.result.fresult=-1.0*val(lparts(ct).part$): t1.result_type=result_float
    compiledline(lineptr)=t1: lineptr+=1 :ct+=1
 case token_string
    t1.result.sresult=lparts(ct).part$: t1.result_type=result_string  
    compiledline(lineptr)=t1: lineptr+=1 :ct+=1
  case token_name  '' we may got token with var or fun # after evaluation (?) 
    getvar(m) : ct+=1
  case token_lpar
    ct+=1
    expr() 
    if lparts(ct).token=token_rpar then ct+=1
    
end select    
end sub

sub getvar(m as integer) 

dim i,j as integer
dim t2 as expr_result
dim varname$,suffix$  as string

varname$=lparts(ct).part$
suffix$=right$(varname$,1)
j=-1

if suffix$="$" then
  for i=0 to svarnum-1
      if svariables(i).name=varname$ then j=i : exit
    next i
  if  j=-1 andalso svarnum<maxvars then   
    svariables(svarnum).name=varname$
    svariables(svarnum).value=""
    j=svarnum
    svarnum+=1 
  endif     
  t2.result_type=fun_getsvar:t2.result.uresult=j
  goto 701
endif  
  
if suffix$="%" then
  for i=0 to uvarnum-1
      if uvariables(i).name=varname$ then j=i : exit
    next i
   if  j=-1 andalso uvarnum<maxvars then   
    uvariables(uvarnum).name=varname$
    uvariables(uvarnum).value=0
    j=uvarnum
    uvarnum+=1
  endif   
    
  t2.result_type=fun_getuvar:t2.result.uresult=j
  goto 701
endif 

if suffix$="!" then
  for i=0 to fvarnum-1
      if fvariables(i).name=varname$ then j=i : exit
    next i
     if  j=-1 andalso fvarnum<maxvars then   
    fvariables(fvarnum).name=varname$
    fvariables(fvarnum).value=0.0
    j=fvarnum
    uvarnum+=1
  endif     
    
  t2.result_type=fun_getfvar:t2.result.uresult=j
  goto 701
endif 

for i=0 to ivarnum-1
  if ivariables(i).name=varname$ then j=i : exit
next i
   if  j=-1 andalso ivarnum<maxvars then   
    ivariables(ivarnum).name=varname$
    ivariables(ivarnum).value=0
    j=ivarnum
    ivarnum+=1
  endif   
t2.result_type=fun_getivar:t2.result.uresult=j

701 
compiledline(lineptr)=t2: lineptr+=1   ' if t2.result.uresult=-1, generate error
if m=-1 then t2.result_type=fun_negative: compiledline(lineptr)=t2: lineptr+=1
end sub

'----------------------------------------------------------------------------------------------------------------------------------------
'--------------------------------------------- End of expression evaluator --------------------------------------------------------------
'----------------------------------------------------------------------------------------------------------------------------------------

'----------------------------------------------------------------------------------------------------------------------------------------
'--------------------------------------------- Runtime functions ------------------------------------------------------------------------ 
'----------------------------------------------------------------------------------------------------------------------------------------

sub execute_line 

'' This executes a line either in immediate mode or loaded from PSRAM in the program executing mode

dim cmd as asub

for lineptr_e=0 to lineptr-1
'print lineptr_e,compiledline(lineptr_e).result_type
cmd=commands(compiledline(lineptr_e).result_type)
cmd
next lineptr_e
end sub


' ------------------- pop and push functions called by do_xxx functions to pop arguments and push results

function pop() as expr_result

dim t1 as expr_result

if stackpointer=0 then
  t1.result_type=result_error
  t1.result.uresult=24
else
  stackpointer -=1
  t1=stack(stackpointer)
endif
return t1
end function 

sub push(t1 as expr_result)

if stackpointer<maxstack then 
  stack(stackpointer)=t1
  stackpointer+=1
' error reporting here  
endif
end sub

' ------------------ push a variable on the stack as an independent operation called by execute_line 

sub do_push
if stackpointer<maxstack then 
  stack(stackpointer)=compiledline(lineptr_e)
  stackpointer+=1
  
endif
end sub

 
  
sub csave_block(address as ulong)

' let it be 1k blocks=256 longs=8 kbits=32 k samples
' we enter it at the state of playing 1 kHz header tone

for i=0 to 63 step 2
  do: loop until lpeek(base+32*7)>32768
  q=lpeek(address+4*i)
     for bit=0 to 31
      if (q and (1 shl bit)) then sample(4*bit)=127: sample(4*bit+1)=128 : sample(4*bit+2)=127 : sample (4*bit+3)=128 else sample(4*bit)=128: sample(4*bit+1)=128 : sample(4*bit+2)=127 : sample (4*bit+3)=127
    next bit  
  do: loop until lpeek(base+32*7)<32768
  q=lpeek(address+4+4*i)
     for bit=0 to 31
      if (q and (1 shl bit)) then sample(128+4*bit)=127: sample(128+4*bit+1)=128 : sample(128+4*bit+2)=127 : sample (128+4*bit+3)=128 else sample(128+4*bit)=128: sample(128+4*bit+1)=128 : sample(128+4*bit+2)=127 : sample (128+4*bit+3)=127
    next bit  
next i
do: loop until lpeek(base+32*7)>32768
for i=0 to 127: if i mod 8 < 4 then sample(i)=127 else sample(i)=128 
next i

do: loop until lpeek(base+32*7)<32768
for i=128 to 255: if i mod 8 < 4 then sample(i)=127 else sample(i)=128 
next i
end sub

sub csave_addtoblock(d as ubyte, force as ubyte)


if force=0 then
  block(blockptr)=d
  blockptr+=1
  if blockptr>=255 then
    csave_block(varptr(block(0)))
    blockptr=0
    waitms(300)
  endif
else
  block(blockptr)=d
  if blockptr<255 then for i=blockptr to 255 : block(i)=0 : next i 
  csave_block(varptr(block(0)))
  blockptr=0
  waitms(300)
endif
end sub  

sub test_csave


dim i,j as integer
dim qqq as ulong
 
dim linebuf(127) as ubyte ' todo : 127
dim name$ as string
dim t1 as expr_result
dim saveptr as ulong
dim header(5) as ulong
'dim fileheader,savestart, saveptr as ulong

t1=pop()
if t1.result_type<>result_string then name$="noname.bas" else name$=t1.result.sresult

' prepare 1 kHz header wave

for i=0 to 255: if i mod 8 < 4 then sample(i)=127 else sample(i)=128 
next i
paula.play8(7,varptr(sample),8000,16384,256,0)
waitms 3000 

blockptr=0
csave_addtoblock($72,0): csave_addtoblock($62,0): csave_addtoblock($61,0): csave_addtoblock($0D,0) ' rba+ver(13)
for i=1 to len(name$): csave_addtoblock(asc(mid$(name$,i,1)),0) : next i : csave_addtoblock(0,0) 
csave_addtoblock($72,0): csave_addtoblock($62,0): csave_addtoblock($73,0): csave_addtoblock($0D,0) ' rbs+ver(13)

saveptr=programstart
do
  psram.read1(varptr(header(0)),saveptr,24)
  psram.read1(varptr(linebuf(0)),header(2),header(3))  
  csave_addtoblock(header(3),0) ' that's always <255
  for i=0 to header(3)-1: csave_addtoblock(linebuf(i),0)    :next i
  saveptr=header(5)
loop until header(5)=$7FFFFFFF
csave_addtoblock(0,1)
waitms(500)
dpoke base+7*32+20,0
end sub

' ----------------- Save the program

sub do_save                           ''' <------------------------ TODO vartables has to be saved too! Or maybe o
dim t1 as expr_result
dim filebuf as ubyte(511)
dim i as integer
dim fileheader,savestart, saveptr as ulong
dim header(5) as ulong
dim linebuf(125) as ubyte

fileheader=$0D616272' rba+ver'

t1=pop() 
if t1.result_type=result_string then
  if t1.result.sresult="" then t1.result.sresult="noname.bas"
  close #9: open currentdir$+t1.result.sresult for output as #9
  put #9,1,fileheader,1
  i=5
  saveptr=programstart
  do
    psram.read1(varptr(header(0)),saveptr,24)
    psram.read1(varptr(linebuf(0)),header(2),header(3))  
    put #9,i,header(3),1 : i+=4
    put #9,i,linebuf(0),header(3) : i+=header(3)
    saveptr=header(5)
  loop until header(5)=$7FFFFFFF
  close #9  
endif  
end sub

'----------------- Load the program
'lo todo: errors while loading
sub do_load
dim t1 as expr_result
dim i, amount as integer
dim header,linelength as ulong
dim line2 as ubyte(125)
dim line2$ as string 

lpoke varptr(line2$),varptr(line2)
t1=pop() 
if t1.result_type=result_string then
  do_new
  if t1.result.sresult="" then t1.result.sresult="noname.bas"
  close #9: open currentdir$+t1.result.sresult for input as #9
  i=5
  get #9,1,header,1
  if header<>$0D616272 then printerror(26) : close #9 : return
  do
    get #9,i,linelength,1,amount : i+=4 : line2(linelength)=0
   if amount=1 then  
      get #9,i,line2(0),linelength : i+=linelength
      line$=line2$: interpret 
  endif 
  loop until amount<1
  close #9  
endif   ' line$="": for j=0 to header(3)-1: line$+=chr$(linebuf(j)): next j
end sub

'----------------- Run the program 

'' line header: linenum major, linenum minor, list start, list length, prev ptr, next ptr

sub do_run

dim runptr as ulong
dim header as ulong(5)
dim key22 : key22=0
let runptr=programstart  
do 
  psram.read1(varptr(header),runptr,24)  
  if header(0)<>-1 then
    psram.read1(varptr(compiledline(0)),runptr+2*compiledslot,header(2)-runptr)
    lineptr=((header(2)-runptr)/compiledslot)-3
    runptr=header(5)
    execute_line
    let key22=kbm.get_key()
    endif
  loop until header(5)=$7FFF_FFFF orelse header(0)=-1 orelse key22=69
end sub

' ---------------  List the program. Todo: it should accept parameters and do "more"

sub do_list
dim t1 as expr_result
dim aend as integer
dim newlist as integer
dim header as ulong(5)
dim linebuf(127) as ubyte
print
let listptr=programstart
do 
  psram.read1(varptr(header),listptr,24)  
  if header(0)<>-1 then
    longfill(linebuf,0,64)
    psram.read1(varptr(linebuf),header(2),header(3))
    v.writeln(varptr(linebuf))  
    listptr=header(5)
    endif

loop until header(5)=$7FFF_FFFF orelse header(0)=-1
end sub

'---------------- Clear the program

sub do_new

pslpoke(0,$FFFFFFFF)
ivarnum=0 : uvarnum=0 : fvarnum=0 : svarnum=0
programstart=0
stackpointer=0
lineptr=0 
programptr=0 :gotoptr=0
lastline=0 : lastlineptr=-1
end sub

'----------------------- goto

sub do_fast_goto
end sub

sub do_slow_goto
end sub

'----------------------- Error processing

sub do_error

dim r as ulong
r=compiledline(lineptr_e).result.uresult
print "Error ";r;": ";errors$(r)
end sub


'------------------ Assigning to a variable  

sub do_assign_i

dim t1 as expr_result
dim varnum as ulong

varnum=compiledline(lineptr_e).result.uresult
t1=pop()
if varnum >=0 then ivariables(varnum).value=t1.result.iresult
end sub

sub do_assign_u

dim t1 as expr_result
dim varnum as ulong

varnum=compiledline(lineptr_e).result.uresult
t1=pop()
if varnum >=0 then uvariables(varnum).value=t1.result.uresult
end sub

sub do_assign_f

dim t1 as expr_result
dim varnum as ulong

varnum=compiledline(lineptr_e).result.uresult
t1=pop()
if varnum >=0 then fvariables(varnum).value=t1.result.fresult
end sub

sub do_assign_s

dim t1 as expr_result
dim varnum as ulong

varnum=compiledline(lineptr_e).result.uresult
t1=pop()
if varnum >=0 then svariables(varnum).value=t1.result.sresult
end sub

' --------------------- Read a variable and push to the stack

sub do_getivar
dim t1 as expr_result
dim r as integer
 
r=ivariables(compiledline(lineptr_e).result.uresult).value
t1.result.iresult=r
t1.result_type=result_int
push t1
end sub

sub do_getuvar
dim t1 as expr_result
dim r as ulong
 
r=uvariables(compiledline(lineptr_e).result.uresult).value
t1.result.uresult=r
t1.result_type=result_uint
push t1
end sub

sub do_getfvar
dim t1 as expr_result
dim r as single
 
r=fvariables(compiledline(lineptr_e).result.uresult).value
t1.result.fresult=r
t1.result_type=result_float
push t1
end sub

sub do_getsvar

dim t1 as expr_result
dim r as string
  
r=svariables(compiledline(lineptr_e).result.uresult).value
t1.result.sresult=r
t1.result_type=result_string

push t1
end sub

'------------------------ Operators 

sub do_plus 

dim t1,t2 as expr_result

t2=pop()
t1=pop()


if t1.result_type=result_uint andalso t2.result_type=result_uint then t1.result.uresult+=t2.result.uresult :goto 1040
if t1.result_type=result_uint andalso t2.result_type=result_int then t1.result.iresult=t1.result.uresult+t2.result.iresult: t1.result_type=result_int :goto 1040
if t1.result_type=result_uint andalso t2.result_type=result_float then t1.result.fresult=cast(single,t1.result.uresult)+t2.result.fresult: t1.result_type=result_float :goto 1040
if t1.result_type=result_int andalso t2.result_type=result_uint then t1.result.iresult+=t2.result.uresult: goto 1040
if t1.result_type=result_int andalso t2.result_type=result_int then t1.result.iresult+=t2.result.iresult:goto 1040
if t1.result_type=result_int andalso t2.result_type=result_float then t1.result.fresult=cast(single,t1.result.iresult)+t2.result.fresult: t1.result_type=result_float :goto 1040
if t1.result_type=result_float andalso t2.result_type=result_uint then t1.result.fresult=t1.result.fresult+cast(single,t2.result.uresult) :goto 1040
if t1.result_type=result_float andalso t2.result_type=result_int then t1.result.fresult=t1.result.fresult+cast(single,t2.result.iresult) :goto 1040
if t1.result_type=result_float andalso t2.result_type=result_float then t1.result.fresult+=t2.result.fresult:goto 1040
if t1.result_type=result_string andalso t2.result_type<>result_string then t1.result.uresult=2 :t1.result_type=result_error:goto 1040
if t2.result_type=result_string andalso t1.result_type<>result_string then t1.result.uresult=2 :t1.result_type=result_error:goto 1040
if t1.result_type=result_string andalso t2.result_type=result_string then t1.result.sresult=t1.result.sresult+t2.result.sresult :goto 1040
t1.result.uresult=4 : t1.result_type=result_error
1040 push t1
end sub

sub do_minus

dim t1,t2 as expr_result

t2=pop()
t1=pop()

if t1.result_type=result_uint andalso t2.result_type=result_uint then 
    if t2.result.uresult<t1.result.uresult then  t1.result.uresult-=t2.result.uresult : goto 1050 else t1.result.iresult=t1.result.uresult-t2.result.uresult : t1.result_type=result_int : goto 1050
    endif
if t1.result_type=result_uint andalso t2.result_type=result_int then t1.result.iresult=t1.result.uresult-t2.result.iresult: t1.result_type=result_int :goto 1050
if t1.result_type=result_uint andalso t2.result_type=result_float then t1.result.fresult=cast(single,t1.result.uresult)-t2.result.fresult: t1.result_type=result_float :goto 1050
if t1.result_type=result_int andalso t2.result_type=result_uint then t1.result.iresult-=t2.result.uresult:goto 1050
if t1.result_type=result_int andalso t2.result_type=result_int then t1.result.iresult-=t2.result.iresult:goto 1050
if t1.result_type=result_int andalso t2.result_type=result_float then t1.result.fresult=cast(single,t1.result.iresult)-t2.result.fresult: t1.result_type=result_float :goto 1050
if t1.result_type=result_float andalso t2.result_type=result_uint then t1.result.fresult=t1.result.fresult-cast(single,t2.result.uresult) :goto 1050
if t1.result_type=result_float andalso t2.result_type=result_int then t1.result.fresult=t1.result.fresult-cast(single,t2.result.iresult) :goto 1050
if t1.result_type=result_float andalso t2.result_type=result_float then t1.result.fresult-=t2.result.fresult:goto 1050
if t1.result_type=result_string orelse t2.result_type=result_string then t1.result.uresult=3: t1.result_type=result_error: goto 1050
t1.result.uresult=5 : t1.result_type=result_error 
1050 push t1
end sub

sub do_and 

dim t1,t2 as expr_result

t2=pop()
t1=pop()
if t1.result_type=result_int then t1.result.uresult=cast(ulong,t1.result.iresult) : t1.result_type=result_uint
if t2.result_type=result_int then t2.result.uresult=cast(ulong,t2.result.iresult) : t2.result_type=result_uint
if t1.result_type=result_string orelse t2.result_type=result_string orelse t1.result_type=result_float orelse t2.result_type=result_float then t1.result.uresult=6: t1.result_type=result_error: goto 1060
t1.result.uresult=t1.result.uresult and t2.result.uresult : goto 1060
t1.result.uresult=7 : t1.result_type=result_error
1060 push t1
end sub

sub do_or 
dim t1,t2 as expr_result

t2=pop()
t1=pop()
if t1.result_type=result_int then t1.result.uresult=cast(ulong,t1.result.iresult) : t1.result_type=result_uint
if t2.result_type=result_int then t2.result.uresult=cast(ulong,t2.result.iresult) : t2.result_type=result_uint
if t1.result_type=result_string orelse t2.result_type=result_string orelse t1.result_type=result_float orelse t2.result_type=result_float then t1.result.uresult=6: t1.result_type=result_error: goto 1070
t1.result.uresult=t1.result.uresult or t2.result.uresult : goto 1070
t1.result.uresult=7 : t1.result_type=result_error 
1070 push t1
end sub

sub do_mul

dim t1,t2 as expr_result

t2=pop()
t1=pop()

if t1.result_type=result_uint andalso t2.result_type=result_uint then t1.result.uresult*=t2.result.uresult :goto 1080
if t1.result_type=result_uint andalso t2.result_type=result_int then t1.result.iresult=t1.result.uresult*t2.result.iresult: t1.result_type=result_int :goto 1080
if t1.result_type=result_uint andalso t2.result_type=result_float then t1.result.fresult=cast(single,t1.result.uresult)*t2.result.fresult: t1.result_type=result_float :goto 1080
if t1.result_type=result_int andalso t2.result_type=result_uint then t1.result.iresult*=t2.result.uresult:goto 1080
if t1.result_type=result_int andalso t2.result_type=result_int then t1.result.iresult*=t2.result.iresult:goto 1080
if t1.result_type=result_int andalso t2.result_type=result_float then t1.result.fresult=cast(single,t1.result.iresult)*t2.result.fresult: t1.result_type=result_float :goto 1080
if t1.result_type=result_float andalso t2.result_type=result_uint then t1.result.fresult=t1.result.fresult*cast(single,t2.result.uresult) :goto 1080
if t1.result_type=result_float andalso t2.result_type=result_int then t1.result.fresult=t1.result.fresult*cast(single,t2.result.iresult) :goto 1080
if t1.result_type=result_float andalso t2.result_type=result_float then t1.result.fresult*=t2.result.fresult:goto 1080
if t1.result_type=result_string orelse t2.result_type=result_string then t1.result.uresult=8: t1.result_type=result_error: goto 1080
t1.result.uresult=9 : t1.result_type=result_error 
1080 push t1
end sub

sub do_div 

dim t1,t2 as expr_result  ' todo: return error at attempting divide by zero

t2=pop()
t1=pop()

if t1.result_type=result_string orelse t2.result_type=result_string then t1.result.uresult=10: t1.result_type=result_error: goto 1090
if t1.result_type=result_float then t1.result_type=result_int : t1.result.iresult=cast(integer,t1.result.fresult)
if t2.result_type=result_float then t2.result_type=result_int : t2.result.iresult=cast(integer,t2.result.fresult)
if t1.result_type=result_uint andalso t2.result_type=result_uint then t1.result.uresult/=t2.result.uresult :goto 1090
if t1.result_type=result_uint andalso t2.result_type=result_int then t1.result.iresult=t1.result.uresult/t2.result.iresult: t1.result_type=result_int :goto 1090
if t1.result_type=result_int andalso t2.result_type=result_uint then t1.result.iresult/=t2.result.uresult :goto 1090
if t1.result_type=result_int andalso t2.result_type=result_int then t1.result.iresult=t1.result.iresult/t2.result.iresult: goto 1090
t1.result.uresult=11 : t1.result_type=result_error 
1090 push t1
end sub

sub do_fdiv 
dim t1,t2 as expr_result  ' todo: return error at attempting divide by zero

t2=pop()
t1=pop()
if t1.result_type=result_string orelse t2.result_type=result_string then t1.result.uresult=10: t1.result_type=result_error: goto 1100
if t1.result_type=result_int then t1.result_type=result_float : t1.result.fresult=cast(single,t1.result.iresult) 
if t1.result_type=result_uint then t1.result_type=result_float : t1.result.fresult=cast(single,t1.result.uresult)
if t2.result_type=result_int then t2.result_type=result_float : t2.result.fresult=cast(single,t2.result.iresult) 
if t2.result_type=result_uint then t2.result_type=result_float : t2.result.fresult=cast(single,t2.result.uresult) 
if t1.result_type=result_float andalso t2.result_type=result_float then t1.result.fresult/=t2.result.fresult: goto 1100
t1.result.uresult=11 : t1.result_type=result_error
1100 push t1
end sub

sub do_mod 

dim t1,t2 as expr_result  

t2=pop()
t1=pop()
if t1.result_type=result_string orelse t2.result_type=result_string then t1.result.uresult=10: t1.result_type=result_error: goto 1110
if t1.result_type=result_float then t1.result_type=result_int : t1.result.iresult=cast(integer,t1.result.fresult)
if t2.result_type=result_float then t2.result_type=result_int : t2.result.iresult=cast(integer,t2.result.fresult)
if t1.result_type=result_uint andalso t2.result_type=result_uint then t1.result.uresult=t1.result.uresult mod t2.result.uresult :goto 1110
if t1.result_type=result_uint andalso t2.result_type=result_int then t1.result.iresult=t1.result.uresult mod t2.result.iresult: t1.result_type=result_int :goto 1110
if t1.result_type=result_int andalso t2.result_type=result_uint then t1.result.iresult=t1.result.iresult mod t2.result.uresult :goto 1110
if t1.result_type=result_int andalso t2.result_type=result_int then t1.result.iresult=t1.result.iresult mod t2.result.iresult: goto 1110
t1.result.uresult=11 : t1.result_type=result_error
1110 push t1
end sub

sub do_shl 
dim t1,t2 as expr_result  

t2=pop()
t1=pop()

if t1.result_type=result_int then t1.result.uresult=cast(ulong,t1.result.iresult) : t1.result_type=result_uint
if t2.result_type=result_int then t2.result.uresult=cast(ulong,t2.result.iresult) : t2.result_type=result_uint
if t1.result_type=result_string orelse t2.result_type=result_string orelse t1.result_type=result_float orelse t2.result_type=result_float then t1.result.uresult=6: t1.result_type=result_error: goto 1120
t1.result.uresult=t1.result.uresult shl t2.result.uresult : goto 1120
t1.result.uresult=7 : t1.result_type=result_error 
1120 push t1
end sub

sub do_shr 

dim t1,t2 as expr_result 
t2=pop()
t1=pop()
if t1.result_type=result_int then t1.result.uresult=cast(ulong,t1.result.iresult) : t1.result_type=result_uint
if t2.result_type=result_int then t2.result.uresult=cast(ulong,t2.result.iresult) : t2.result_type=result_uint
if t1.result_type=result_string orelse t2.result_type=result_string orelse t1.result_type=result_float orelse t2.result_type=result_float then t1.result.uresult=6: t1.result_type=result_error: goto 1130
t1.result.uresult=t1.result.uresult shr t2.result.uresult : goto 1130
t1.result.uresult=7 : t1.result_type=result_error
1130 push t1
end sub

sub do_power 

dim t1,t2 as expr_result 
t2=pop()
t1=pop()


if t1.result_type=result_string orelse t2.result_type=result_string then t1.result.uresult=12: t1.result_type=result_error: goto 1140
if t1.result_type=result_int then t1.result_type=result_float : t1.result.fresult=cast(single,t1.result.iresult) 
if t1.result_type=result_uint then t1.result_type=result_float : t1.result.fresult=cast(single,t1.result.uresult) 
if t2.result_type=result_int then t2.result_type=result_float : t2.result.fresult=cast(single,t2.result.iresult) 
if t2.result_type=result_uint then t2.result_type=result_float : t2.result.fresult=cast(single,t2.result.uresult) 
if t1.result_type=result_float andalso t2.result_type=result_float then t1.result.fresult=t1.result.fresult^t2.result.fresult: goto 1140
t1.result.uresult=13 : t1.result_type=result_error 
1140 push t1
end sub

' -------------------   convert a variable on the top of stack to integer

sub do_converttoint

dim t1 as expr_result 
dim a1,r as integer
t1=pop() 
select case t1.result_type
  case result_int: a1=t1.result.iresult : r=result_int
  case result_uint: a1=t1.result.uresult : r=result_int
  case result_float: a1=round(t1.result.fresult) : r=result_int
  case result_string: a1=val(t1.result.sresult) :r=result_int
  case result_error: a1=0: r=t1.result.uresult
  case else : a1=0 : r=1

end select
t1.result.iresult=a1 : t1.result_type=r : push t1 

end sub

'' ----------------------------- Graphics related runtime procedures --------------------------------------

'' ----------------------------- Clear the screen

sub do_cls
cls(): print
end sub

'' ----------------------------- Set a color # from the palette to plot/draw

sub do_color
dim t1 as expr_result
t1=pop()
plot_color=t1.result.iresult
end sub

' ----------------------------- Plot a point, set starting point to draw a line

sub do_plot
dim t1,t2 as expr_result 
t2=pop()
t1=pop()
plot_x=t1.result.iresult
plot_y=t2.result.iresult
v.putpixel(plot_x,plot_y,plot_color) 
end sub

' --------------------------- Draw a line to point set by plot or previous draw, set a new starting point

sub do_draw
dim t1,t2 as expr_result 
t2=pop()
t1=pop()
v.draw(plot_x,plot_y,t1.result.iresult,t2.result.iresult,plot_color) 
plot_x=t1.result.iresult
plot_y=t2.result.iresult
end sub

' -------------------------- Draw a filled circle at x,y and radius r

sub do_fcircle
dim t1,t2,t3 as expr_result 

t3=pop()
t2=pop()
t1=pop()
v.fcircle(t1.result.iresult,t2.result.iresult,t3.result.iresult,plot_color) 
end sub

'' ----------------------------- Text related runtime procedures --------------------------------------

' ------------------------- Print to the screem

sub do_print  

dim t1,t2 as expr_result
dim r as integer
 
r=0
t1=pop() 
if t1.result_type=print_mod_comma orelse t1.result_type=print_mod_semicolon then r=t1.result_type :  t1=pop()
if t1.result_type=print_mod_empty then r=t1.result_type 
if t1.result_type=result_error then printerror(t1.result.uresult): goto 811

if r=print_mod_comma  then
  if t1.result_type=result_int then print t1.result.iresult,
  if t1.result_type=result_uint then print t1.result.uresult,
  if t1.result_type=result_float then print t1.result.fresult,
  if t1.result_type=result_string then print t1.result.sresult,
endif  
if r=print_mod_semicolon then 
  if t1.result_type=result_int then print t1.result.iresult;
  if t1.result_type=result_uint then print t1.result.uresult;
  if t1.result_type=result_float then print t1.result.fresult;
  if t1.result_type=result_string then print t1.result.sresult;
endif
if r=0 then 
  if t1.result_type=result_int then print t1.result.iresult
  if t1.result_type=result_uint then print t1.result.uresult
  if t1.result_type=result_float then print t1.result.fresult
  if t1.result_type=result_string then print t1.result.sresult
endif 
if r=print_mod_empty then print
'waitms(10)
811 end sub

''----------------------------------------------------------------------------------------------------
''------------------ Initialization procedures -------------------------------------------------------
''----------------------------------------------------------------------------------------------------

''--------------------------- Command function pointers

sub init_commands

commands(token_plus)=@do_plus 
commands(token_minus)=@do_minus 
commands(token_or)=@do_or 
'commands(token_xor)=@do_xor 
commands(token_mul)=@do_mul
commands(token_fdiv)=@do_fdiv
commands(token_and)=@do_and
commands(token_div)=@do_div
commands(token_mod)=@do_mod
commands(token_shl)=@do_shl
commands(token_shr)=@do_shr
commands(token_power)=@do_power
'commands(token_at)=@do_at
'commands(token_inc)=@do_inc
commands(fun_getivar)=@do_getivar
commands(fun_getuvar)=@do_getuvar
commands(fun_getfvar)=@do_getfvar
commands(fun_getsvar)=@do_getsvar
commands(fun_pushu)=@do_push 
commands(fun_pushi)=@do_push  
commands(fun_pushf)=@do_push  
commands(fun_pushs)=@do_push  
commands(fun_assign_u)=@do_assign_u
commands(fun_assign_i)=@do_assign_i
commands(fun_assign_f)=@do_assign_f
commands(fun_assign_s)=@do_assign_s  
commands(print_mod_empty)=@do_push
commands(print_mod_comma)=@do_push
commands(print_mod_semicolon)=@do_push
commands(token_cls)=@do_cls
commands(token_new)=@do_new
commands(token_plot)=@do_plot
commands(token_draw)=@do_draw
commands(token_print)=@do_print
'commands(token_circle)=@do_circle
commands(token_fcircle)=@do_fcircle
'commands(token_box)=@do_box
'commands(token_frame)=@do_frame
commands(token_color)=@do_color
'commands(token_for)=@do_for
'commands(token_next)=@do_next
commands(token_list)=@do_list
commands(token_run)=@do_run
commands(token_error)=@do_error
commands(token_fast_goto)=@do_fast_goto
commands(token_slow_goto)=@do_slow_goto
commands(fun_converttoint)=@do_converttoint 
commands(token_csave)=@test_csave
commands(token_save)=@do_save
commands(token_load)=@do_load
end sub

''--------------------------------Error strings -------------------------------------

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
errors$(19)="No more variable slots."
errors$(20)="Variable not found."
errors$(21)="Comma expected."
errors$(22)="Comma or semicolon expected."
errors$(23)="Unknown command."
errors$(24)="Stack underflow."
errors$(25)="Cannot execute goto in the immediate mode."
errors$(26)="Cannot load from this file."
end sub
        
sub printerror(err as integer)

v.write("Error " ): v.write(v.inttostr(err)) : v.write(": ")  : v.writeln(errors$(err))
end sub

'' ------------------------------- Hardware start/stop/initialization 

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

'' ------------------------------- Convenient psram peek/poke

sub pslpoke(addr as ulong,value as ulong)
psram.filllongs(addr,value,1,0)
end sub

sub pspoke(addr as ulong,value as ulong)
psram.fillbytes(addr,value,1,0)
end sub

function pspeek(adr as ulong) as ubyte
dim res as ubyte
psram.read1(varptr(res),adr,1)
return res
end function

function pslpeek(adr as ulong) as ulong
dim res as ulong
psram.read1(varptr(res),adr,4)
return res
end function

'' ------------------------------- More convenient video driver function aliases

sub position(x,y)
v.setcursorpos(x,y)
end sub

sub waitvbl
  v.waitvbl(1)
end sub

'' ------------------------------- USB keyboard scan to char translator

function scantochar(key)

select case (key shr 8) and 255
case 0     : return keys(4*(key and 127))
case 2,32  : return keys(4*(key and 127)+1) ' shift
case 64	   : return keys(4*(key and 127)+2) ' RAlt
case 66,96 : return keys(4*(key and 127)+3) ' RAlt+shift

end select
end function

'' ------------------------------- USB keyboard scan to char translation table

dim shared as ubyte keys(511)={
 0,0,0,0, 			 
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
 0,0,0,0}


 '' ------------------------------- Atari 8-bit type keyboard click and overflow sounds
 
asm shared
atari_spl file "atari.spl"
atari2_spl file "atari2.spl" '1758
end asm
