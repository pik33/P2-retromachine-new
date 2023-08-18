const HEAPSIZE=256000
'#define PSRAM4
#define PSRAM16

#ifdef PSRAM16
const _clkfreq = 337000000
dim v as class using "hg010b.spin2"
dim psram as class using "psram.spin2"
#endif

#ifdef PSRAM4
const _clkfreq = 337000000
dim v as class using "hg009-4.spin2"
dim psram as class using "psram4.spin2"
#endif

dim kbm as class using "usbnew.spin2"
dim paula as class using "audio093b-8-sc.spin2"

#include "dir.bi"

''-----------------------------------------------------------------------------------------
''---------------------------------- Constants --------------------------------------------
''-----------------------------------------------------------------------------------------

const ver$="P2 Retromachine BASIC version 0.20"
const ver=20
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
const token_gt=15
const token_lt=16
const token_comma=17
const token_semicolon=18
const token_ear=19
const token_rpar=20
const token_lpar=21
const token_colon=22

const fun_getvar=17  ' at runtime, reuse non-function tokens with functions that have no compile time tokens
const fun_getivar=17  ' at runtime, reuse non-function tokens with functions that have no compile time tokens
const fun_getuvar=18
const fun_getfvar=19
const fun_getsvar=20
const fun_negative=21
const fun_converttoint=22
const fun_assign=23
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

const token_le=38
const token_ge=39
const token_inc=40
const token_dec=41
const token_ne=42

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
const token_find_goto=83
const token_rnd=84
const token_pinwrite=85
const token_waitms=86
const token_waitvbl=87
const token_if=88
const token_brun=89
const token_else=90
const token_then=91
const token_beep=92
const token_dir=93
const token_paper=94
const token_ink=95
const token_font=96
const token_mode=97
const token_gettime=98
const token_mouse=99
const token_mousex=100
const token_mousey=101
const token_mousek=102
const token_mousew=103
const token_cursor=104
const token_click=105
const token_stick=106
const token_sin=107
const token_defsprite=108
const token_sprite=109
const token_strig=110
const token_getpixel=111
const token_waitclock=112
const token_fill=113
const token_dim=114


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
const result_array=token_dim
const result_error=token_error

' -----------------------------max number of variables and stack depth
const maxvars=1023       
const maxstack=512
const maxfor=128

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
  dim ulresult as ulongint       ' make an 8 byte placeholder for in64 and double
  dim twowords(1) as ulong	 ' and allow single word access to it
end union
  
class expr_result		' general variable, not only expression result :) 
  dim result as aresult
  dim result_type as ulong  
end class

class variable
  dim name as string
  dim value as aresult
  dim vartype as ulong
end class  

class goto_entry
  dim source as ulong
  dim dest as ulong
end class

class for_entry
  dim lineptr as ulong ' line that will be executed after next
  dim cmdptr as ulong  ' command# in this line
  dim varnum as integer  ' variable to modify and check
  dim stepval as integer
  dim endval as integer
end class  

type parts as part(125)         ' parts to split the line into, line has 125 chars max
type asub as sub()		' sub type to make a sub table

''-----------------------------------------------------------------------------------------
''---------------------------------- Global variables ------------------------------------
''-----------------------------------------------------------------------------------------

dim variables as variable(maxvars)
dim varnum as integer
dim lparts as parts

dim audiocog,videocog,usbcog,pscog,pslock as integer
dim base as ulong
dim mbox as ulong
dim ansibuf(3) as ubyte
dim line$ as string
dim fullline$ as string
dim cont as ulong

dim plot_color,plot_x,plot_y as integer
dim editor_spaces as integer
dim paper,ink, font as integer
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
dim stringtable(maxvars) as string
dim stringptr as integer
dim currentdir$ as string
dim fortable(maxfor) as for_entry

dim sample(255) as ubyte ' for csave
dim block(1023) as ubyte ' for csave
dim blockptr as ulong
dim runptr,runptr2,oldrunptr as ulong
dim inrun as ulong
dim runheader as ulong(5)
dim fortop as integer
dim free$ as string
dim keyclick as integer
dim housekeeper_cog as integer
dim housekeeper_stack as integer(128)
dim mousex,mousey,mousek, mousew as ulong
dim stick(6) as ulong
dim strig(6) as ulong
dim sprite(15) as ubyte pointer
dim hkcnt as ulong
dim memtop as ulong

'----------------------------------------------------------------------------
'-----------------------------Program start ---------------------------------
'----------------------------------------------------------------------------

startpsram
startvideo
audiocog,base=paula.start(0,0,0)
waitms(50)
dpoke base+20,16384
usbcog=kbm.start()
kbm.mouse_set_limits(1023,575)
kbm.mouse_set_outptr(varptr(v.spr1ptr)+16*12+4)
v.setspriteptr(16,@mouse)
v.setspritesize(16,32,32) 
kbm.mouse_move(512,288)
housekeeper_cog=cpu(housekeeper(),@housekeeper_stack(0)) : hkcnt=0
editor_spaces=2
paper=147: ink=154 : font=4
plot_color=ink : plot_x=0: plot_y=0
keyclick=1
compiledslot=sizeof(test)

init_commands
init_error_strings
do_new


cls(ink, paper)
'v.setfontfamily(4) 				' use ST Mono font
v.setfontfamily(font) 				' use ST Mono font
v.setleadingspaces(2)
mount "/sd", _vfs_open_sdcard()
chdir "/sd/bas"

currentdir$="/sd/bas"

position 2*editor_spaces,1 : print ver$
free$=decuns$(v.buf_ptr)+" BASIC bytes free" : print free$
position 2*editor_spaces,4 : print "Ready"
'hubset( %1_000001__00_0001_1010__1111_1011)


'-------------------------------------------------------------------------------------------------------- 
'-------------------------------------- MAIN LOOP -------------------------------------------------------
'--------------------------------------------------------------------------------------------------------

do
waitvbl

'' Do key repeat

let key=kbm.get_key() 
let leds=kbm.ledstates() 'numlock 1 capslock 2 scrollock 4
if key>0 andalso key<4 andalso keyclick=1 then paula.play(7,@atari2_spl,44100,16384,0,1758): waitms(10): paula.stop(7)
if key>3 andalso key<$80000000 andalso (key and 255) <$E0 then let key2=key : let rpt=1 : let key3=key2
if key>$80000000 then let rptcnt=0 : let rpt=0
if key=0 andalso rpt=1 then rptcnt+=1
if key<$80000000 then if rptcnt=25 then key3=key2 : rptcnt=21


if key3<>0 then
  if keyclick=1 then paula.play(7,@atari_spl,44100,16384,1684) 
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
  if key4>0 andalso key4<127 andalso v.cursor_x=254 andalso keyclick=1 then paula.play(7,@atari2_spl,44100,16384,0,1758): waitms(300): paula.stop(0) 'end of line reached
 
  'tab
  if (key3 and 255) = 43 andalso v.cursor_x>=240 andalso keyclick=1 then paula.play(0,@atari2_spl,44100,16384,0,1758): waitms(300): paula.stop(0)
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
      interpret: line$="" :let t1=getct()-t1 :rpt=0: rptcnt=0
    endif 

  key3=0 
  endif

loop



'----------------------------------- this is the end of the main loop ------------------------------------------------------------------

sub housekeeper

do
  do: loop until v.vblank=1 : hkcnt+=1 :gethdi
  waitms(5) : hkcnt+=1 : gethdi
  waitms(5) : hkcnt+=1 : gethdi
  waitms(5) : hkcnt+=1 : gethdi
loop
end sub


sub gethdi
dim  dummy,i,j,x,y as ulong

  mousex,mousey=kbm.mouse_xy()
  dummy,mousew=kbm.mouse_scroll()
  mousek=kbm.mouse_buttons()
  
  i=0:
  for j=0 to 6
    if kbm.hidpad_id(j)>0 then
      x=kbm.hidpad_axis(j,0) : y=kbm.hidpad_axis(j,1)
      x=1+(x+49152) shr 15 : y=1+(y+49152) shr 15 : stick(i)=x+(y shl 2) 
      strig(i)=kbm.hidpad_buttons(j) 
      i=i+1
    endif
  next j  
for j=i to 6 : stick(j)=0 : strig(j)=0 : next j 
''' sticks:
'''''                   5     6     7
'''''			9    10    11
'''''		       13    14    15
end sub

sub waitclock

dim c as ulong
c=hkcnt
do: loop until hkcnt<>c
end sub

'---------------------------------------------------------------------------------------------------------------------------------------
'----------------------------------- The line interpreter/tokenizer --------------------------------------------------------------------
'---------------------------------------------------------------------------------------------------------------------------------------

sub interpret

 
dim i,j,k,q 
dim result as expr_result
dim etype as integer
dim separators(125)
dim linenum as ulong
dim err as integer

' ---------------------------------------------------  Pass 1: Split the line to parts, detect and concatenate strings

fullline$=line$: cont=-1  : linenum=0 : lineptr=0 : err=0

108 for i=0 to 125: separators(i)=0 :next i
for i=0 to 125: lparts(i).part$="": next i

' 1a : extract the first command, split the line to first command and the rest

line$=trim$(line$):let d$="" : let l=len(line$) 
if l=0 then goto 101
let d=instr(1,line$,":"): if d>0 andalso d<len(line$)  then let rest$=trim$(right$(line$,len(line$)-d)):line$=trim$(left$(line$,d-1)) else rest$="" 

if cont=-1 andalso rest$<>"" then cont=0 : goto 107       	' this is the first and not last part
if cont=-1 andalso rest$="" then cont=3 : goto 107		' this is the first AND last part
if cont=4 andalso rest$<>"" then cont=1 : goto 107		' this is not the first and not the last part
if cont=4 andalso rest$="" then cont=2 :goto 107		' this is the last, and not the first, part

' 1b: find separators

107
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
   for j=i+1 to k: lparts(j)=lparts(j+1): next j  
   i-=1 : k-=1 ' do not move i if concatenated
 endif
 i+=1 : loop until i>=k 
 
' 1e2: concatenate >=, <=, ++, --, +=, *=, -=, /=, ^=, <>
 
i=0 : do
  let s1$=lparts(i).part$ : let s2$=lparts(i+1).part$
  if ((s1$=">" orelse s1$=">" orelse s1$="+" orelse s1$="-" orelse s1$="*" orelse s1$="/" orelse s1$="^") andalso s2$="=") orelse (s1$="+" andalso s2$="+") orelse (s1$="-" andalso s2$="-") orelse (s1$="<" andalso s2$=">") then
    lparts(i).part$=s1$+s2$
    for j=i+1 to k : lparts(j)=lparts(j+1) : next j
   i-=1 : k-=1 ' do not move i if concatenated
 endif
 i+=1 : loop until i>=k  
 
' 1f : now remove parts that are spaces

for i=0 to k: lparts(i).part$=trim$(lparts(i).part$): next i

i=0
do 
  if len(lparts(i).part$)=0 then 
    if i=k-1 then k-=1  :  exit
    if i<k-1 then 
      for j=i to k-2 : lparts(j)=lparts(j+1): next j: k-=1  
      if i>0 then i-=1 
    endif  
  endif
 i+=1: loop until i>k-1

' 1g: lowercase all that is not a string

for j=0 to k-1
  if left$(lparts(j).part$,1)<>"""" orelse right$(lparts(j).part$,1)<>"""" then lparts(j).part$=lcase$(lparts(j).part$) 
next j

'                                                         for i=0 to k-1 : print lparts(i).part$,: next i : print

for i=0 to k: lparts(i).token=-1: next i

'-------------------------------------------------------- Pass 2: Tokenize the line

if len(lparts(0).part$)=0 then goto 101				' empty line, nothing to do

' 2a find part types 

for i=0 to k-1
lparts(i).token=isseparator(lparts(i).part$): if lparts(i).token>0 then goto 102
lparts(i).token=isoperator(lparts(i).part$): if lparts(i).token>0 then goto 102
lparts(i).token=isassign(lparts(i).part$) : if lparts(i).token>0 then goto 102
lparts(i).token=iscommand(lparts(i).part$): if lparts(i).token>0 then goto 102
lparts(i).token=isfunction(lparts(i).part$): if lparts(i).token>0 then goto 102

let b1=isnum(lparts(i).part$):let b2=isint(lparts(i).part$):let b3=isdec(lparts(i).part$)
if b1 andalso b2 andalso b3 then lparts(i).token=token_decimal : goto 102 					' pure decimal for line num
if b1 andalso b2 andalso (not b3) then lparts(i).token=token_integer : goto 102 				' integer
if b1 andalso (not b2) andalso (not b3) then lparts(i).token=token_float :goto 102 				' float

if isstring(lparts(i).part$) then 
    lparts(i).token=token_string : lparts(i).part$=mid$(lparts(i).part$,2,len(lparts(i).part$)-2) :goto 102	' string, get rid of ""!
    stringtable(stringptr)=lparts(i).part$ : stringptr+=1							' and protect it from disappearing
endif
if isname(lparts(i).part$) then lparts(i).token=token_name : goto 102						' name
102 next i 

lparts(k).token=token_end : lparts(k).part$="": tokennum=k

 '                                      					 	for i=0 to k: print lparts(i).token,lparts(i).part$ : next i

' process the case when simple load or save is called without ""

if (lparts(0).part$="load" orelse lparts(0).part$="save" orelse lparts(0).part$="brun") andalso lparts(1).token=token_name andalso lparts(2).token=token_end then lparts(1).token=token_string
if (lparts(0).part$="mouse" orelse lparts(0).part$="cursor" orelse lparts(0).part$="click") andalso lparts(1).token=token_name andalso lparts(2).token=token_end then 
  if lparts(1).part$="on" then lparts(1).part$="1" :lparts(1).token=token_decimal
  if lparts(1).part$="off" then lparts(1).part$="0" :lparts(1).token=token_decimal
endif									

'2b determine a type of the line
if isdec(lparts(0).part$) then linenum=val%(lparts(0).part$)

if linenum>0 andalso k=1 andalso cont=3 then deleteline(linenum) : goto 104

if linenum>0  andalso (cont=0 orelse cont=3) andalso lparts(2).token<>token_eq  then  
  err= compile(linenum,0,cont) ': print "called compile with cont=";cont, "line$=";line$
  if err<>0 then printerror(err): goto 104
  if rest$<>"" then  line$=rest$ : cont=4 : goto 108 else goto 104
endif
      							
if linenum>0 andalso (cont=1 orelse cont=2) andalso lparts(1).token<>token_eq  then 
  err= compile(linenum,0,cont) ': print "called compile with cont=";cont, "line$=";line$
  if err<>0 then printerror(err): goto 104
  if rest$<>"" then line$=rest$: cont=4 : goto 108 else goto 104  	
endif
							 
if linenum>0 andalso (cont=0 orelse cont=3) andalso lparts(2).token=token_eq then  
  compile_assign(linenum,0,cont)': print "called compile_assign with cont=";cont, "line$=";line$
  if rest$<>"" then line$=rest$: cont=4 : goto 108 else goto 104
endif
    							 
if linenum>0 andalso (cont=1 orelse cont=2) andalso lparts(1).token=token_eq then 
  compile_assign(linenum,0,cont) ': print "called compile_assign with cont=";cont, "line$=";line$
  if rest$<>"" then line$=rest$: cont=4 : goto 108 else goto 104  								'<-- TODO: add a line to a program
endif

if lparts(0).token=token_name andalso lparts(1).token=token_eq then compile_assign(0) : goto 103    					' assign a variable
if lparts(0).token=token_name andalso lparts(1).token=token_rpar then print " User functions and arrays not yet implemented" : goto 101

' if we are here, this is not a program line to add, so try to execute this

err=compile(0) : '' execute(0) ' print "  this is a command to execute"  ''' param=line to compile
103  'for i=0 to lineptr: print compiledline(i).result_type;" ";compiledline(i).result.uresult, : next i
if err=0 then execute_line() else printerror(err)
if rest$<>"" then line$=rest$:  goto 108 

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
  case "="   : return token_eq ' the compiler then will determine what type is to assign
  case ">="  : return token_ge   
  case "<="  : return token_le  
  case "<"   : return token_lt   
  case ">"   : return token_gt 
  case "<>"   : return token_ne 
  case "++"   : return token_inc 
  case "--"   : return token_dec 


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
  case ">"   : return token_gt
  case "<"   : return token_lt
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
  case "brun"	     : return token_brun
  case "pinwrite"    : return token_pinwrite
  case "waitms"	     : return token_waitms
  case "waitvbl"     : return token_waitvbl
  case "if"	     : return token_if
  case "else"	     : return token_else
  case "then"	     : return token_then
  case "beep"	     : return token_beep
  case "dir"	     : return token_dir
  case "for"	     : return token_for
  case "next"	     : return token_next
  case "paper"	     : return token_paper
  case "ink"	     : return token_ink
  case "font"	     : return token_font
  case "mode"	     : return token_mode
  case "mouse"	     : return token_mouse
  case "cursor"	     : return token_cursor
  case "click"	     : return token_click
  case "defsprite"   : return token_defsprite
  case "sprite"	     : return token_sprite
  case "waitclock"   : return token_waitclock
  case "fill"        : return token_fill
  case "dim"	     : return token_dim
  case else          : return 0  
end select
end function

function isfunction(s as string) as ubyte

select case s
  case "rnd"           	: return token_rnd
  case "mousex"        	: return token_mousex
  case "mousey"        	: return token_mousey
  case "mousek"        	: return token_mousek
  case "mousew"        	: return token_mousew
  case "gettime"       	: return token_gettime
  case "sin"       	: return token_sin
  case "stick"       	: return token_stick
  case "strig"       	: return token_strig
  case "getpixel"     	: return token_getpixel
  case else		: return 0
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

dim lineptr2,oldsearchptr,searchptr as ulong
dim header as ulong(5) 


searchptr=programstart

do
  psram.read1(varptr(header),searchptr,24)
  lineptr2=searchptr
  searchptr=header(5)
loop until header(0)>=aline orelse header(5)=$7FFF_FFFF  ' we have a line that number is >= new line


if header(0)<>aline then return -1

pslpoke(lineptr2,$FFFF_FFFF) ' flag the deleted line

if header(5)=$7FFF_FFFF andalso header(4)=$FFFF_FFFF then ' this is one and only line in the program
  programstart=0 : programptr=0 : lastline=0 : lastlineptr=-1  : lineptr=0 
  pslpoke(0,$FFFFFFFF) : pslpoke 16,$FFFFFFFF : pslpoke 20,$7FFFFFFF
endif

if header(5)=$7FFF_FFFF andalso header(4)<>$FFFF_FFFF then ' this is the last, and not first, line of the program
  pslpoke(header(4)+20,$7FFF_FFFF) ' unlink the previous line
  lastlineptr=header(4)            ' keep last line pointer to avoid searching while sequentially adding a new line
  lastline=pslpeek(header(4))
  return 0
endif   

if header(5)<>$7FFF_FFFF andalso header(4)=$FFFF_FFFF then ' this is the first line, but not the last
'   print "deleted first line"
  pslpoke(header(5)+16,$FFFF_FFFF) 
  programstart=header(5) ' adjust the program start to point on the first new line
  return 0
endif

if header(5)<>$7FFF_FFFF andalso header(4)<>$FFFF_FFFF then ' the line is not first and not last
   pslpoke(header(5)+16,header(4))  
   pslpoke(header(4)+20, header(5))
   return 0
endif   



' now find if the deleted line was a target for goto and replace fast_goto with find_goto

lineptr2=searchptr
searchptr=programstart

do
  psram.read1(varptr(header),searchptr,24)
  lineptr2=searchptr
  searchptr=header(5)
loop until header(0)>=aline orelse header(5)=$7FFF_FFFF  ' we have a line that number is >= new line



end function

sub save_line

dim llength,llength2,llength3 as ulong

llength=compiledslot*(lineptr+1)
llength2=len (fullline$): if llength2 mod 4 <>0 then llength2=4*((llength2/4)+1)
llength3=llength+llength2
ucompiledline(2)=programptr+llength
ucompiledline(3)=llength2 
   
psram.write(varptr(compiledline),programptr,llength)
psram.write(lpeek(varptr(fullline$)),programptr+llength,llength2)
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

function compile_immediate(linetype as ulong) as integer

dim cmd,err,vars as ulong
dim t3 as expr_result

' linetype=cont+1, linetype=0 immediate
'  : 1: this is the first part of the line that will continue
' 2 - this is the continuation of the line
' 3 - this is the last continued line
' 4 - this is the one and only part

err=0
cmd=0
if linetype=0 then cmd=lparts(0).token : ct=1 : lineptr=0 
if linetype=2 orelse linetype=3 then cmd=lparts(0).token : ct=1 ' don't set lineptr
if linetype=4 orelse linetype=1 then cmd=lparts(1).token : ct=2 : lineptr=2
if linetype=5 then cmd=lparts(ct).token : ct+=1 ' continued after if/else
vars=0
'print cmd
451 select case cmd
  case token_cls      : compile_nothing()   'no params, do nothing, only add a command to the line, but case needs something to do after 
  case token_new      : compile_nothing()   
  case token_list     : vars=compile_fun_varp()   
  case token_run      : compile_nothing()   
  case token_plot     : err=compile_fun_2p()   
  case token_draw     : err=compile_fun_2p()   
  case token_circle   : err=compile_fun_3p()  
  case token_fcircle  : err=compile_fun_3p()  
  case token_box      : err=compile_fun_4p()  
  case token_frame     : err=compile_fun_4p()  
  case token_color    : err=compile_fun_1p()  
  case token_print    : err=compile_print()  : goto 450
  case token_fast_goto     : if linetype>0 then compile_goto()  : goto 450 else printerror(25) : goto 450
  case token_csave    : compile_fun_1p()  
  case token_save    : compile_fun_1p()  'todo compile_str_fun_1p
  case token_load    : compile_fun_1p()  'todo compile_str_fun_1p
  case token_brun    : compile_fun_1p()  'todo compile_str_fun_1p
  case token_pinwrite    : compile_int_fun_2p()
  case token_waitms    : compile_int_fun_1p()
  case token_waitvbl    : compile_nothing()
  case token_waitclock    : compile_nothing()
  case token_if      :   compile_if() :goto 450
  case token_for     :   compile_for() :goto 450
  case token_next     :   compile_next() :goto 450

  case token_else    :   compile_else() : goto 450
  case token_beep	: err=compile_fun_2p()
  case token_dir	:compile_nothing
  case token_paper	:compile_fun_1p
  case token_ink	:compile_fun_1p
  case token_font	:compile_fun_1p
  case token_mode	:compile_fun_1p
  case token_mouse	:compile_fun_1p
  case token_cursor	:compile_fun_1p
  case token_click	:compile_fun_1p
  case token_sprite	:compile_fun_3p
  case token_defsprite	:compile_fun_5p
  case token_fill	:compile_fun_4p
  case token_dim	:compile_dim: goto 450
  case else	      : compile_unknown() : goto 450

end select

t3.result_type=cmd : t3.result.uresult=vars : compiledline(lineptr)=t3:  lineptr+=1
450 if linetype=0 orelse linetype=3 orelse linetype=4 then compiledline(lineptr).result_type=token_end ' end token if the last part or imm

' print "In compile_immediate:" : for i=0 to lineptr: print compiledline(i).result_type;" ";compiledline(i).result.uresult : next i
return err
end function


sub compile_immediate_assign(linetype as ulong)

dim t1 as expr_result
dim i,j as integer
dim  suffix2$ as string
dim varname2$ as string

t1.result_type=result_error : t1.result.uresult=0
i=-1: j=-1

if linetype=0 then varname2$=lparts(0).part$: ct=2 : lineptr=0 
if linetype=2 orelse linetype=3 then varname2$=lparts(0).part$ : ct=2 ' don't set lineptr
if linetype=4 orelse linetype=1 then varname2$=lparts(1).part$ : ct=3 : lineptr=2
if linetype=5 then varname2$=lparts(ct).part$ : ct+=2 ' continued after if/else
 
'print "Called compile immediate assign with linetype",linetype, "varname=",varname2$, "lineptr=", lineptr

suffix2$=right$(varname2$,1)
expr()


if varnum>0 then
  for i=0 to varnum-1
    if variables(i).name=varname2$ then j=i : exit
  next i
endif
if  j=-1 andalso varnum<maxvars then   
  variables(varnum).name=varname2$
  j=varnum
  varnum+=1
endif
t1.result.uresult=j: t1.result_type=fun_assign  


compiledline(lineptr)=t1:  lineptr+=1 
 if linetype=0 orelse linetype=3 orelse linetype=4 then compiledline(lineptr).result_type=token_end

'for i=0 to lineptr: print compiledline(i).result_type;" ";compiledline(i).result.uresult : next i
'print "at exit lineptr=",lineptr
end sub

' ------------------ compile the line that is calling a command 

function compile (alinemajor as ulong, alineminor=0 as ulong, cont=0 as ulong)

dim err as integer
'line header: num major, num minor,list start, list length, prev, next. That implements 2-way list of program lines 
' num_minor bit 31: the line is goto target. If deleted, a proper record(s) has to be added to goto list
 
'  print "called compile with line= "; alinemajor;" and cont= "; cont 
if alinemajor=0 then err=compile_immediate(0) : return err  

ucompiledline(0)=alinemajor
ucompiledline(1)=alineminor


' cont: 0: this is the first part of the line that will continue
' 1 - this is the continuation of the line
' 2 - this is the last continued line
' 3 - this is the ome and only part


err=compile_immediate(cont+1) 
if err=0 then
  if cont=3 orelse cont=2 then 
    if alinemajor >lastline then 
      add_line_at_end(alinemajor)
    else
      deleteline(alinemajor)  
      if alinemajor>lastline then add_line_at_end(alinemajor) else insertline(alinemajor) ' yes I know that's not optimal    
    endif 
  endif   

endif 
return err
end function

' ------------------ compile the line that is assigning to a variable

sub compile_assign (alinemajor as ulong, alineminor=0 as ulong, cont=0 as ulong)  

'  print "called compile_assign  with line= "; alinemajor;" and cont= "; cont 
if alinemajor=0 then compile_immediate_assign(0) : return  

ucompiledline(0)=alinemajor
ucompiledline(1)=alineminor

compile_immediate_assign(cont+1) 

if cont=3 orelse cont=2 then 
  if alinemajor >lastline then 
    add_line_at_end(alinemajor)
  else
    deleteline(alinemajor)  
    if alinemajor>lastline then add_line_at_end(alinemajor) else insertline(alinemajor)   
  endif 
endif 
end sub

' --------------- Helper compile functions 

sub compile_nothing
end sub

sub compile_error(errno)
dim t1 as expr_result 
t1.result_type=token_error : t1.result.uresult=errno
compiledline(lineptr)=t1: lineptr+=1 
end sub


sub compile_unknown() 

dim t1 as expr_result 
t1.result_type=token_error : t1.result.uresult=23
compiledline(lineptr)=t1: lineptr+=1 
end sub

function compile_converttoint() as integer

dim t1 as expr_result 
dim err as integer
err=0
t1.result.uresult=0
err=expr()
if err=0 then
  t1.result_type=fun_converttoint
  compiledline(lineptr)=t1: lineptr+=1 
  return 0
else
  return err
endif      
end function




function compile_fun_1p() as ulong


expr()
return 0
end function

function compile_int_fun_1p() as ulong
 
 dim err as integer
let err=compile_converttoint()  
return err
end function


function compile_fun_2p() as ulong

expr()
if lparts(ct).token<> token_comma then return 21 else ct+=1 ' todo error
expr()
end function

function compile_fun_3p() as ulong

expr()
if lparts(ct).token<> token_comma then return 21 else ct+=1 ' todo error
expr()
if lparts(ct).token<> token_comma then return 21 else ct+=1 ' todo error
expr()

end function

function compile_fun_4p() as ulong

expr()
if lparts(ct).token<> token_comma then return 21 else ct+=1 ' todo error
expr()
if lparts(ct).token<> token_comma then return 21 else ct+=1 ' todo error
expr()
if lparts(ct).token<> token_comma then return 21 else ct+=1 ' todo error
expr()

end function


function compile_fun_5p() as ulong

expr()
if lparts(ct).token<> token_comma then return 21 else ct+=1 ' todo error
expr()
if lparts(ct).token<> token_comma then return 21 else ct+=1 ' todo error
expr()
if lparts(ct).token<> token_comma then return 21 else ct+=1 ' todo error
expr()
if lparts(ct).token<> token_comma then return 21 else ct+=1 ' todo error
expr()
end function

function compile_fun_varp() as ulong ' parameter # on top of the stack

dim t1 as expr_result
dim i as integer
i=0
if lparts(ct).token<>token_end then
  do
  expr()
   i+=1
   if lparts(ct).token<> token_comma then exit loop else ct+=1
  loop 
endif
return i
end function


function compile_int_fun_2p() as ulong
 
  dim err as integer
err=compile_converttoint() : if err>0 then return err
if lparts(ct).token<> token_comma then return 21 else ct+=1 ' todo error
err=compile_converttoint() 
return err
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
  expr()  ': print "In compile_print token= "; lparts(ct).token; " part$= "; lparts(ct).part$ :
  if lparts(ct).token=token_comma then t1.result_type=print_mod_comma : compiledline(lineptr)=t1:  lineptr+=1 : t1.result_type=token_print : compiledline(lineptr)=t1:  lineptr+=1
  if lparts(ct).token=token_semicolon then  t1.result_type=print_mod_semicolon : compiledline(lineptr)=t1:  lineptr+=1 : t1.result_type=token_print : compiledline(lineptr)=t1:  lineptr+=1
  if lparts(ct).token=token_end then t1.result_type=token_print : compiledline(lineptr)=t1:  lineptr+=1
  if lparts(ct).token <>token_comma andalso lparts(ct).token <>token_semicolon andalso lparts(ct).token <>token_end then  return 22
  ct+=1  
loop until lparts(ct).token=token_end orelse ct>=tokennum
return 0
end function

function compile_if() as ulong  

dim t1 as expr_result
dim cmd as ulong

'print "In compile_if"
compile_int_fun_1p()
cmd=lparts(ct).token : ct+=1
'print cmd ' : if cmd<> token_then then print "Expected then" else print "Found then"
t1.result_type=token_if : compiledline(lineptr)=t1:  lineptr+=1
if isassign(lparts(ct+1).part$) then compile_immediate_assign(5) else compile_immediate(5)


return 0
end function

function compile_else() as ulong  

dim t1 as expr_result
dim cmd as ulong

t1.result_type=token_else : compiledline(lineptr)=t1:  lineptr+=1
if isassign(lparts(ct+1).part$) then compile_immediate_assign(5) else compile_immediate(5)


return 0
end function


function compile_dim() as ulong  

 
dim t1 as expr_result
dim cmd as ulong
dim dims(2) as ulong
dim i,j,l,m as integer
dim varname2$ as string

dims(0)=1: dims(1)=1: dims(2)=1

if isname(lparts(ct).part$) then 
  varname2$=lparts(ct).part$ 
  j=-1
  if varnum>0 then
    print varnum
    for i=0 to varnum-1
      if variables(i).name=varname2$ then j=i : exit
    next i

  endif 
  if j<>-1 then printerror (42) : return 42
  if lparts(ct+1).part$ <>"(" then printerror(43) : return 43
  l=ct+2 : m=0 : do
    print lparts(l).part$, isdec(lparts(l).part$)
    if isdec(lparts(l).part$) then 
      dims(m)=val%(lparts(l).part$) : m+=1  
    else 
      printerror (17)  : return(17)
    endif  
    if (lparts(l+1).part$<>"," andalso lparts(l+1).part$<>")" ) then printerror (44)  : return(44)
  l+=2
  loop until lparts(l-1).part$=")" orelse m>2
  if m>3 then printerror(45): return(45)
else
  printerror(46): return 46
endif
print dims(0),dims(1),dims(2)

' to do

'- allocate a new variable
'- type: result_arraay
'- compute size: dim*dim*dim*12 (sizeof expr_result) + header ( size,size, size)
' - lower memtop, var uresult=memtop - pointer to an array

' getvar - ??? 




 ' if  j=-1 andalso varnum<maxvars then   
  '  variables(varnum).name=varname2$
  '  variables(varnum).type=result_array
' allocate space on memtop. Todo: check if the space exists
    
  '  j=varnum
 '   varnum+=1
'endif

'
 '  j=-1 
 '  do: j=j+1 : loop until variables(j).name=lparts(ct).part$ orelse j>=varnum
   


'compile_immediate_assign(5) else compile_error(32) : return 32
'' after this we should have fun_assign_i or fun_assign_u with var# as uresult.
't1=compiledline(lineptr-1): if t1.result_type<>fun_assign  then compile_error(34) : return 34
'varnum=t1.result.uresult
'if lparts(ct).part$<>"to" then  compile_error(33) : return 33
'ct+=1
'expr()  ' there is "to" value pushed on the stack
'if lparts(ct).part$="step" then 
'ct+=1
'expr()
'else
'compiledline(lineptr).result_type=result_int : compiledline(lineptr).result.iresult=1 : lineptr+=1
'endif
'compiledline(lineptr).result_type=result_int : compiledline(lineptr).result.iresult=varnum :lineptr+=1
'compiledline(lineptr).result_type=token_for : compiledline(lineptr).result.iresult=0 :lineptr+=1
return 0
end function


function compile_for() as ulong  

 
dim t1 as expr_result
dim cmd,varnum as ulong



if isassign(lparts(ct+1).part$) then compile_immediate_assign(5) else compile_error(32) : return 32
'' after this we should have fun_assign_i or fun_assign_u with var# as uresult.
t1=compiledline(lineptr-1): if t1.result_type<>fun_assign  then compile_error(34) : return 34
varnum=t1.result.uresult
if lparts(ct).part$<>"to" then  compile_error(33) : return 33
ct+=1
expr()  ' there is "to" value pushed on the stack
if lparts(ct).part$="step" then 
ct+=1
expr()
else
compiledline(lineptr).result_type=result_int : compiledline(lineptr).result.iresult=1 : lineptr+=1
endif
compiledline(lineptr).result_type=result_int : compiledline(lineptr).result.iresult=varnum :lineptr+=1
compiledline(lineptr).result_type=token_for : compiledline(lineptr).result.iresult=0 :lineptr+=1


return 0
end function


sub do_for()
' on the stack: varnum, step, endval

dim t1 as expr_result
dim i as integer
fortop+=1
'i=-1: do: i+=1 : loop until fortable(i).varnum= -1 orelse i>= maxfor
'if i> maxfor then printerror(36) : return
t1=pop() : fortable(fortop).varnum=t1.result.iresult
t1=pop() : fortable(fortop).stepval=t1.result.iresult
t1=pop() : fortable(fortop).endval=t1.result.iresult
if compiledline(lineptr_e).result_type=token_end then
' end of line after for, set the pointer to the start of the next line
fortable(fortop).lineptr=runptr
fortable(fortop).cmdptr=0
else
fortable(fortop).lineptr=oldrunptr
fortable(fortop).cmdptr=lineptr_e+1
'fortop=i ' to speedup next
endif
end sub

' now do_next todo

sub do_next()

dim t1 as expr_result
dim varnum as integer

t1=pop() :varnum=t1.result.uresult
if fortable(fortop).varnum<>t1.result.uresult then printerror(37) : return
variables(varnum).value.iresult+=fortable(fortop).stepval 
if fortable(fortop).stepval>0 then
  if variables(varnum).value.iresult>fortable(fortop).endval then fortop-=1 : return ' do nothing 
else
  if variables(varnum).value.iresult<fortable(fortop).endval then fortop -=1 : return ' do nothing 
endif
' if not returned, goto pointer
runptr=fortable(fortop).lineptr
runptr2=fortable(fortop).cmdptr
lineptr_e=lineptr-1
'runheader(5)=0
' todo: detect for at the same line and don't reload
end sub


function compile_next() as ulong

dim t1 as expr_result
dim cmd,i,j as ulong
dim varname$ as string
dim suffix$ as string

varname$=lparts(ct).part$ 
'suffix$=right$(varname$,1)
'if varname$="" orelse suffix$="$" orelse suffix$="!" orelse suffix$="#" then  compile_error(34) : printerror(34) : return 34
if varnum=0 then compile_error(35)  : return 35
j=-1
for i=0 to varnum-1
  if variables(i).name=varname$ then j=i : exit
next i
if j=-1 then compile_error(35) : return 35
compiledline(lineptr).result_type=result_int : compiledline(lineptr).result.iresult=j :lineptr+=1
compiledline(lineptr).result_type=token_next : compiledline(lineptr).result.iresult=0 :lineptr+=1
return 0
end function


' next: we  have to find the variable in the table, compile pushvar, then next
' do_for: push its own pointer, varnum, step, end on the for stack. var init is already compiled before
' do_next: find the entry with the varnum. Add step to varnum. Compare to the end. If step>0, check >, else check <. If not end, goto forptr (how?) 


function compile_goto( ) as ulong

dim gotoline, gotoptr,oldgotoptr as integer
dim gotoheader as ulong(5)

if lparts(ct).token=token_decimal andalso lparts(ct+1).token=token_end then 

  gotoline=val%(lparts(ct).part$) 
  compiledline(lineptr).result_type=token_fast_goto
' now find a pointer to goto
  gotoptr=programstart
  do
    psram.read1(varptr(gotoheader),gotoptr,24)  : 
    if gotoheader(0)<>$FFFFFFFF then
      oldgotoptr=gotoptr
      gotoptr=gotoheader(5)
    endif
  loop until gotoheader(5)=$7FFF_FFFF orelse gotoheader(0)=-1 orelse gotoheader(0)=gotoline
  if gotoheader(0)=gotoline then
    compiledline(lineptr).result.twowords(0)=oldgotoptr ' we got the pointer
    compiledline(lineptr).result.twowords(1)=gotoline
'    print "compiled fast goto to line ";gotoline, " at pointer "; oldgotoptr
  else
    compiledline(lineptr).result.twowords(0)=$80000000
    compiledline(lineptr).result.twowords(1)=gotoline
    compiledline(lineptr).result_type=token_find_goto
 '  print "compiled find goto to line ";gotoline, " at pointer "; oldgotoptr

  endif  
  lineptr+=1
else
   print "We have a slow goto, todo  "
endif
' if not, there is a slow goto. Call converttoint to get an int value from expression, then do_slow_goto
' Do_slow_goto searches a line pointer list to find the linenum and pointer, then do the goto  
return 0
end function




'---------------------------------------------------------------------------------------------------------------------------------------
'------------------------------ End of the precompiler  --------------------------------------------------------------------------------
'---------------------------------------------------------------------------------------------------------------------------------------


'---------------------------------------------------------------------------------------------------------------------------------------
'---------------------------- Compile time expression decoder/evaluator ----------------------------------------------------------------
'---------------------------------------------------------------------------------------------------------------------------------------


function expr() as integer 

' On input: ct = current token position
' On output: expression result value and a new ct


dim t3 as expr_result
dim op as integer
dim rt as integer

op=lparts(ct).token : if op=token_end then t3.result.uresult=29 : t3.result_type=result_error : compiledline(lineptr)=t3 : lineptr+=1: return 29
t3.result.uresult=0
rt=addsub()             			' call higher priority operator check. It will itself call getval/getvar if no multiplies or divides
op = lparts(ct).token				' that idea is from github adamdunkels/ubasic
do while (op = token_eq orelse op = token_gt orelse op = token_lt orelse op=token_ge orelse op=token_le orelse op=token_ne)
  ct+=1
  rt=addsub() 
  t3.result_type=op: compiledline(lineptr)=t3: lineptr+=1
  op = lparts(ct).token
  loop
return 0  
end function


'' todo: use propre ops. At getvar and getconst level we know the rt. If both are ints, do int op. If one are float, compile converttofloat, do float op. If both strings, call string op. If string ant int combined, compile err

function addsub() as integer 

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
  return 0
end function

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
  case else
    getfun(m) : ct+=1
      
    
end select    
end sub

sub getfun(m as integer) ' todo - functions return type, todo" fun can have expr list after it
dim oldct,numpar as ulong
dim t2 as expr_result
 ' if lparts(ct+1).token=token_lpar then oldct=ct: ct+=1: expr()
oldct=ct
numpar=0
  
if lparts(ct+1).token=token_lpar then
  ct+=1 											' omit this lpar, this is for expr list
  do
    ct+=1  											': print "In getfun, ct=",ct,"lparts(ct).token=",lparts(ct).token, "part$=",lparts(ct).part$
    if lparts(ct).token=token_lpar then ct+=1 : expr() : ct+=1 else expr()
    numpar+=1
   '' if lparts(ct).token=token_comma then print "in getfun, comma found"
  loop until lparts(ct).token=token_rpar    orelse lparts(ct).token=token_end  ' generate error if end
  
 'if lparts(ct).token=token_rpar then print "in getfun, rpar found, numpar=",numpar
 'if lparts(ct).token=token_end then print "in getfun, end found, numpar=",numpar
endif  

t2.result.uresult=numpar

t2.result_type=lparts(oldct).token  ' todo here: expression lists..... 
compiledline(lineptr)=t2: lineptr+=1   ' if t2.result.uresult=-1, generate error

  
if m=-1 then t2.result_type=fun_negative: compiledline(lineptr)=t2: lineptr+=1
 end sub
  
  
sub getvar(m as integer) 

dim i,j as integer
dim t2 as expr_result
dim varname$,suffix$  as string

varname$=lparts(ct).part$
suffix$=right$(varname$,1)
j=-1

for i=0 to varnum-1
  if variables(i).name=varname$ then j=i : exit
next i
if  j=-1 andalso varnum<maxvars then   
  variables(varnum).name=varname$
  variables(varnum).value.iresult=0
  variables(varnum).vartype=result_int
  j=varnum
  varnum+=1 
endif     
t2.result_type=fun_getvar:t2.result.uresult=j

/'


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
'/
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

function execute_line (astart=0 as integer) as integer

'' This executes a line either in immediate mode or loaded from PSRAM in the program executing mode

dim cmd as asub
runptr2=0
for lineptr_e=astart to lineptr-1
'print lineptr_e,compiledline(lineptr_e).result_type
'if compiledline(lineptr_e).result_type>255 then printerror(31): return ' this should never happen and it is an interpreter error. Added to debug
''let tt=getct()
cmd=commands(compiledline(lineptr_e).result_type and 255)
cmd
''let tt=getct()-tt: print compiledline(lineptr_e).result_type, tt
next lineptr_e
return runptr2
end function


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

sub push(t1 as expr_result )

'print "In push: "; t1.result_type
'print "In push: "; t1.result.uresult

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

if pslpeek(programstart)=$FFFFFFFF then printerror(27): return
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
'waitms(500)
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
if pslpeek(programstart)=$FFFFFFFF then printerror(27): return
if t1.result_type=result_string then
  if t1.result.sresult="" then t1.result.sresult="noname.bas"
  close #9: open currentdir$+"/"+t1.result.sresult for output as #9
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
dim i, r, amount as integer
dim header,linelength as ulong
dim line2 as ubyte(125)
dim line2$ as string 

lpoke varptr(line2$),varptr(line2)
t1=pop() 
if t1.result_type=result_string then
  do_new
  if t1.result.sresult="" then t1.result.sresult="noname.bas" 
'   print currentdir$+"/"+t1.result.sresult
  close #9: open currentdir$+"/"+t1.result.sresult for input as #9
    r=geterr() : if r then print "System error ";r;": ";strerror$(r) :close #9 : return
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
else
  printerror(30)  ' line$="": for j=0 to header(3)-1: line$+=chr$(linebuf(j)): next j
endif
end sub

'----------------- Run the program 

'' line header: linenum major, linenum minor, list start, list length, prev ptr, next ptr

sub do_run

dim key22 : key22=0

let runptr=programstart  : runptr2=0 : oldrunptr=-1
if inrun>0 then 
  psram.read1(varptr(runheader),runptr,24)  
  return
endif
inrun=1
psram.read1(varptr(runheader),runptr,24) 
if runheader(0)=$FFFFFFFF then inrun=0: return 
do 
  if runptr<>oldrunptr then
    psram.read1(varptr(runheader),runptr,24) 					        ' : let tt=getct() - tt : print "loaded header, time=", tt
    psram.read1(varptr(compiledline(0)),runptr+2*compiledslot,runheader(2)-runptr)    	' : let tt=getct()-tt : : print "loaded compiledline, time=", tt' todo: avoid 2 psram reads. Make a buf for he line or something
      											'for i=0 to lineptr : print compiledline(i).result_type, : next i
    lineptr=((runheader(2)-runptr)/compiledslot)-3  					' : let tt=getct()-tt :: print "computed lineptr, time="; tt ' todo: keep the line ptr
    oldrunptr=runptr	 	 							' : let tt=getct()-tt :  print "got a new header, time="; tt
  endif
runptr=runheader(5)	  							' : let tt=getct()-tt :  print "got a new header, time="; tt
runptr2=execute_line(runptr2)										' :  let tt=getct()-tt : :print "excuted a line "; runheader(0), "time="; tt
loop until runptr=$7FFF_FFFF orelse kbm.get_key()=$106 
if runheader(5)<>$7FFF_FFFF then 
  if keyclick=1 then paula.play(7,@atari_spl,44100,16384,1684)  
  print "Stopped at line ";runheader(0)
endif
inrun=0
end sub

' ---------------  List the program. Todo: it should accept parameters and do "more"

sub do_list
dim numpar, startline,endline
dim t1 as expr_result
dim aend as integer
dim newlist as integer
dim header as ulong(5)
dim linebuf(127) as ubyte

startline=0 : endline=$7FFFFFFF
numpar=compiledline(lineptr_e).result.uresult
if numpar=1 then t1=pop() : startline=converttoint(t1)
if numpar=2 then t1=pop() : endline=converttoint(t1) : t1=pop() : startline=converttoint(t1)


print
let listptr=programstart
do 
  psram.read1(varptr(header),listptr,24) ': print header(0),header(1),header(2),header(3),header(4),header(5), programstart
  if header(0)<> $FFFFFFFF then
    longfill(linebuf,0,64)
    psram.read1(varptr(linebuf),header(2),header(3))
    if header(0)>=startline andalso header(0)<=endline then v.writeln(varptr(linebuf))  
    listptr=header(5)
    endif

loop until header(5)=$7FFF_FFFF orelse header(0)=-1
end sub

'---------------- Clear the program

sub do_new

pslpoke(0,$FFFFFFFF)
varnum=0
programstart=0 :runptr=0 : runptr2=0
stackpointer=0
lineptr=0 
programptr=0 : stringptr=0
lastline=0 : lastlineptr=-1 :fortop=0
for i=0 to maxfor: fortable(i).varnum=-1 : next i
for i=0 to 15: if sprite(i)<> nil then v.setspritesize(i,0,0) : delete(sprite(i))
memtop=v.buf_ptr
v.setspritesize(17,8,16)
next i
end sub

'----------------------- goto
sub do_fast_goto

dim testptr,flag as ulong

testptr=compiledline(lineptr_e).result.uresult
flag=pslpeek(testptr)' :print " In goto:",flag , testptr : waitms(1000)
if flag=compiledline(lineptr_e).result.twowords(1) then
  runptr=testptr
  lineptr_e=lineptr-1
  if runheader(5)=$7FFF_FFFF  then runheader(5)=0
else
  do_find_goto  
endif  
end sub 




sub do_find_goto

dim gotoline,gotoptr,oldgotoptr as integer
dim gotoheader(5) as ulong

gotoline=compiledline(lineptr_e).result.twowords(1)
 ' print "find goto"                                                                     'print gotoline 
gotoptr=programstart
do
  psram.read1(varptr(gotoheader),gotoptr,24)  : 
  if gotoheader(0)<>$FFFFFFFF then
    oldgotoptr=gotoptr
    gotoptr=gotoheader(5)
  endif
  loop until gotoheader(5)=$7FFF_FFFF orelse gotoheader(0)=-1 orelse gotoheader(0)=gotoline

if gotoheader(0)=gotoline then
    compiledline(lineptr_e).result.uresult=oldgotoptr ' we got the pointer
    compiledline(lineptr_e).result_type=token_fast_goto
    psram.write(varptr(compiledline(lineptr_e)),oldrunptr+(2+lineptr_e)*compiledslot,compiledslot)   
    do_fast_goto
  else
     printerror(38)
  endif  
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


sub do_assign

dim t1 as expr_result
dim varnum as ulong

varnum=compiledline(lineptr_e).result.uresult
t1=pop() 
variables(varnum).value=t1.result : variables(varnum).vartype=t1.result_type
end sub


' --------------------- Read a variable and push to the stack

sub do_getvar
dim t1 as expr_result
t1.result=variables(compiledline(lineptr_e).result.uresult).value
t1.result_type=variables(compiledline(lineptr_e).result.uresult).vartype
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


sub do_eq

dim t1,t2 as expr_result 
t2=pop()
t1=pop()

if t1.result_type=result_string andalso t2.result_type=result_string then t1.result.uresult=(t1.result.sresult=t2.result.sresult) : goto 1150
if t1.result_type=result_float andalso t2.result_type=result_float then t1.result.uresult=(t1.result.fresult=t2.result.fresult) : goto 1150
if t1.result_type=result_float andalso t2.result_type=result_int then t1.result.uresult=(t1.result.fresult=t2.result.iresult) : goto 1150
if t1.result_type=result_float andalso t2.result_type=result_uint then t1.result.uresult=(t1.result.fresult=t2.result.uresult) : goto 1150
if t1.result_type=result_int andalso t2.result_type=result_float then t1.result.uresult=(t1.result.iresult=t2.result.fresult) : goto 1150
if t1.result_type=result_int andalso t2.result_type=result_int then t1.result.uresult=(t1.result.iresult=t2.result.iresult) : goto 1150
if t1.result_type=result_int andalso t2.result_type=result_uint then t1.result.uresult=(t1.result.iresult=t2.result.uresult) : goto 1150
if t1.result_type=result_uint andalso t2.result_type=result_float then t1.result.uresult=(t1.result.uresult=t2.result.fresult) : goto 1150
if t1.result_type=result_uint andalso t2.result_type=result_int then t1.result.uresult=(t1.result.uresult=t2.result.iresult) : goto 1150
if t1.result_type=result_uint andalso t2.result_type=result_uint then t1.result.uresult=(t1.result.uresult=t2.result.uresult) : goto 1150
t1.result.uresult=0
1150 t1.result_type=result_int 
push t1
end sub

sub do_gt

dim t1,t2 as expr_result 
t2=pop()
t1=pop()

if t1.result_type=result_string andalso t2.result_type=result_string then t1.result.uresult=(t1.result.sresult>t2.result.sresult) : goto 1160
if t1.result_type=result_float andalso t2.result_type=result_float then t1.result.uresult=(t1.result.fresult>t2.result.fresult) : goto 1160
if t1.result_type=result_float andalso t2.result_type=result_int then t1.result.uresult=(t1.result.fresult>t2.result.iresult) : goto 1160
if t1.result_type=result_float andalso t2.result_type=result_uint then t1.result.uresult=(t1.result.fresult>t2.result.uresult) : goto 1160
if t1.result_type=result_int andalso t2.result_type=result_float then t1.result.uresult=(t1.result.iresult>t2.result.fresult) : goto 1160
if t1.result_type=result_int andalso t2.result_type=result_int then t1.result.uresult=(t1.result.iresult>t2.result.iresult) : goto 1160
if t1.result_type=result_int andalso t2.result_type=result_uint then t1.result.uresult=(t1.result.iresult>t2.result.uresult) : goto 1160
if t1.result_type=result_uint andalso t2.result_type=result_float then t1.result.uresult=(t1.result.uresult>t2.result.fresult) : goto 1160
if t1.result_type=result_uint andalso t2.result_type=result_int then t1.result.uresult=(t1.result.uresult>t2.result.iresult) : goto 1160
if t1.result_type=result_uint andalso t2.result_type=result_uint then t1.result.uresult=(t1.result.uresult>t2.result.uresult) : goto 1160
t1.result.uresult=0
1160 t1.result_type=result_int 
push t1
end sub

sub do_lt

dim t1,t2 as expr_result 
t2=pop()
t1=pop()

if t1.result_type=result_string andalso t2.result_type=result_string then t1.result.uresult=(t1.result.sresult<t2.result.sresult) : goto 1170
if t1.result_type=result_float andalso t2.result_type=result_float then t1.result.uresult=(t1.result.fresult<t2.result.fresult) : goto 1170
if t1.result_type=result_float andalso t2.result_type=result_int then t1.result.uresult=(t1.result.fresult<t2.result.iresult) : goto 1170
if t1.result_type=result_float andalso t2.result_type=result_uint then t1.result.uresult=(t1.result.fresult<t2.result.uresult) : goto 1170
if t1.result_type=result_int andalso t2.result_type=result_float then t1.result.uresult=(t1.result.iresult<t2.result.fresult) : goto 1170
if t1.result_type=result_int andalso t2.result_type=result_int then t1.result.uresult=(t1.result.iresult<t2.result.iresult) : goto 1170
if t1.result_type=result_int andalso t2.result_type=result_uint then t1.result.uresult=(t1.result.iresult<t2.result.uresult) : goto 1170
if t1.result_type=result_uint andalso t2.result_type=result_float then t1.result.uresult=(t1.result.uresult<t2.result.fresult) : goto 1170
if t1.result_type=result_uint andalso t2.result_type=result_int then t1.result.uresult=(t1.result.uresult<t2.result.iresult) : goto 1170
if t1.result_type=result_uint andalso t2.result_type=result_uint then t1.result.uresult=(t1.result.uresult<t2.result.uresult) : goto 1170
t1.result.uresult=0
1170 t1.result_type=result_int 
push t1
end sub

sub do_ge

dim t1,t2 as expr_result 
t2=pop()
t1=pop()

if t1.result_type=result_string andalso t2.result_type=result_string then t1.result.uresult=(t1.result.sresult>=t2.result.sresult) : goto 1180
if t1.result_type=result_float andalso t2.result_type=result_float then t1.result.uresult=(t1.result.fresult>=t2.result.fresult) : goto 1180
if t1.result_type=result_float andalso t2.result_type=result_int then t1.result.uresult=(t1.result.fresult>=t2.result.iresult) : goto 1180
if t1.result_type=result_float andalso t2.result_type=result_uint then t1.result.uresult=(t1.result.fresult>=t2.result.uresult) : goto 1180
if t1.result_type=result_int andalso t2.result_type=result_float then t1.result.uresult=(t1.result.iresult>=t2.result.fresult) : goto 1180
if t1.result_type=result_int andalso t2.result_type=result_int then t1.result.uresult=(t1.result.iresult>=t2.result.iresult) : goto 1180
if t1.result_type=result_int andalso t2.result_type=result_uint then t1.result.uresult=(t1.result.iresult>=t2.result.uresult) : goto 1180
if t1.result_type=result_uint andalso t2.result_type=result_float then t1.result.uresult=(t1.result.uresult>=t2.result.fresult) : goto 1180
if t1.result_type=result_uint andalso t2.result_type=result_int then t1.result.uresult=(t1.result.uresult>=t2.result.iresult) : goto 1180
if t1.result_type=result_uint andalso t2.result_type=result_uint then t1.result.uresult=(t1.result.uresult>=t2.result.uresult) : goto 1180
t1.result.uresult=0
1180 t1.result_type=result_int 
push t1
end sub

sub do_le

dim t1,t2 as expr_result 
t2=pop()
t1=pop()

if t1.result_type=result_string andalso t2.result_type=result_string then t1.result.uresult=(t1.result.sresult<=t2.result.sresult) : goto 1190
if t1.result_type=result_float andalso t2.result_type=result_float then t1.result.uresult=(t1.result.fresult<=t2.result.fresult) : goto 1190
if t1.result_type=result_float andalso t2.result_type=result_int then t1.result.uresult=(t1.result.fresult<=t2.result.iresult) : goto 1190
if t1.result_type=result_float andalso t2.result_type=result_uint then t1.result.uresult=(t1.result.fresult<=t2.result.uresult) : goto 1190
if t1.result_type=result_int andalso t2.result_type=result_float then t1.result.uresult=(t1.result.iresult<=t2.result.fresult) : goto 1190
if t1.result_type=result_int andalso t2.result_type=result_int then t1.result.uresult=(t1.result.iresult<=t2.result.iresult) : goto 1190
if t1.result_type=result_int andalso t2.result_type=result_uint then t1.result.uresult=(t1.result.iresult<=t2.result.uresult) : goto 1190
if t1.result_type=result_uint andalso t2.result_type=result_float then t1.result.uresult=(t1.result.uresult<=t2.result.fresult) : goto 1190
if t1.result_type=result_uint andalso t2.result_type=result_int then t1.result.uresult=(t1.result.uresult<=t2.result.iresult) : goto 1190
if t1.result_type=result_uint andalso t2.result_type=result_uint then t1.result.uresult=(t1.result.uresult<=t2.result.uresult) : goto 1190
t1.result.uresult=0
1190 t1.result_type=result_int 
push t1
end sub


sub do_ne

dim t1,t2 as expr_result 
t2=pop()
t1=pop()

if t1.result_type=result_string andalso t2.result_type=result_string then t1.result.uresult=(t1.result.sresult<>t2.result.sresult) : goto 1192
if t1.result_type=result_float andalso t2.result_type=result_float then t1.result.uresult=(t1.result.fresult<>t2.result.fresult) : goto 1192
if t1.result_type=result_float andalso t2.result_type=result_int then t1.result.uresult=(t1.result.fresult<>t2.result.iresult) : goto 1192
if t1.result_type=result_float andalso t2.result_type=result_uint then t1.result.uresult=(t1.result.fresult<>t2.result.uresult) : goto 1192
if t1.result_type=result_int andalso t2.result_type=result_float then t1.result.uresult=(t1.result.iresult<>t2.result.fresult) : goto 1192
if t1.result_type=result_int andalso t2.result_type=result_int then t1.result.uresult=(t1.result.iresult<>t2.result.iresult) : goto 1192
if t1.result_type=result_int andalso t2.result_type=result_uint then t1.result.uresult=(t1.result.iresult<>t2.result.uresult) : goto 1192
if t1.result_type=result_uint andalso t2.result_type=result_float then t1.result.uresult=(t1.result.uresult<>t2.result.fresult) : goto 1192
if t1.result_type=result_uint andalso t2.result_type=result_int then t1.result.uresult=(t1.result.uresult<>t2.result.iresult) : goto 1192
if t1.result_type=result_uint andalso t2.result_type=result_uint then t1.result.uresult=(t1.result.uresult<>t2.result.uresult) : goto 1192
t1.result.uresult=0
1192 t1.result_type=result_int 
push t1
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

function converttoint (t1 as expr_result) as integer

select case t1.result_type
  case result_int:  return t1.result.iresult
  case result_uint: return t1.result.uresult
  case result_float: return round(t1.result.fresult)
  case result_string: return val(t1.result.sresult)
  case else: return 0
end select
end function

sub do_rnd

dim t1 as expr_result
dim numpar as ulong

numpar=compiledline(lineptr_e).result.uresult
if numpar>1 then print "rnd: "; : printerror(39) : return
if numpar=0 then
  t1.result_type=result_uint
  t1.result.uresult=getrnd()
  push t1
else
  t1=pop()
  if t1.result_type=result_int orelse t1.result_type=result_uint then
'   print " In do_rnd got int  param"
   t1.result.uresult=getrnd() mod t1.result.uresult
   t1.result_type=result_uint   
   push t1  
 else if t1.result_type=result_float then
'   print " In do_rnd got float  param"
   t1.result.fresult=(t1.result.fresult/1048576.0)*(getrnd() mod 1048576)
   t1.result_type=result_float   
   push t1    
  else 
    print "rnd: "; : printerror(40) 
    push t1
  endif  
endif  
end sub

sub do_sin

dim t1 as expr_result
dim numpar as ulong

numpar=compiledline(lineptr_e).result.uresult
if numpar>1 orelse numpar=0 then print "sin: "; : printerror(39) : return
t1=pop()
if t1.result_type=result_int orelse t1.result_type=result_uint then
  t1.result.fresult=sin(3.141592654*((t1.result.iresult mod 360)/180.0))
  t1.result_type=result_float   
  push t1  
else if t1.result_type=result_float then
  t1.result.fresult=sin(3.141592654*(t1.result.fresult/180.0))
  t1.result_type=result_float   
  push t1    
else 
  print "sin: "; : printerror(40) 
  push t1
endif  

end sub

sub do_stick

dim t1 as expr_result
dim numpar as ulong

numpar=compiledline(lineptr_e).result.uresult
if numpar>1 then print "stick: "; : printerror(39) : return

if numpar=0 then t1.result.uresult=stick(0) : t1.result_type=result_uint : push t1 : return

t1=pop()
if t1.result_type=result_int orelse t1.result_type=result_uint then  
  q=t1.result.uresult
  if q<7 then 
    t1.result.uresult=stick(q) : t1.result_type=result_uint : push t1 : return 
  else 
     printerror(41) : return
  endif
  
else
  printerror(41) 
endif    
  
end sub

sub do_strig

dim t1 as expr_result
dim numpar as ulong

numpar=compiledline(lineptr_e).result.uresult
if numpar>1 then print "strig: "; : printerror(39) : return

if numpar=0 then t1.result.uresult=strig(0) : t1.result_type=result_uint : push t1 : return

t1=pop()
if t1.result_type=result_int orelse t1.result_type=result_uint then  
  q=t1.result.uresult
  if q<7 then 
    t1.result.uresult=strig(q) : t1.result_type=result_uint : push t1 : return 
  else 
     printerror(41) : return
  endif
  
else
  printerror(41) 
endif    
  
end sub

sub do_getpixel

dim t1,t2 as expr_result
dim numpar,a1,a2 as ulong

numpar=compiledline(lineptr_e).result.uresult
if numpar<>2 then print "getpixel: "; : printerror(39) : return

t2=pop()
t1=pop()
a1=converttoint(t1) : a2=converttoint(t2)
t1.result.uresult=pspeek(v.buf_ptr+a1+1024*a2)
t1.result_type=result_uint
push t1

 
end sub



sub do_defsprite
dim t1,t2,t3,t4,t5 as expr_result 
dim a1,a2,a3,a4,a5 as integer

t5=pop()
t4=pop()
t3=pop()
t2=pop()
t1=pop()

' do convert, defsprite is not a racing command
a1=converttoint(t1) : a2=converttoint(t2) : a3=converttoint(t3) : a4=converttoint(t4) : a5=converttoint(t5)
 
' todo: check parameters for linits
if sprite(a1)<> nil then delete(sprite(a1))
sprite(a1)=new ubyte(a4*a5-1)
for y=a3 to a3+a5-1
  for x=a2 to a4+a2-1
    sprite(a1,(x-a2)+(y-a3)*(a4))=pspeek(v.buf_ptr+x+1024*y)
  next x
next y
v.setspriteptr(a1,sprite(a1))
v.setspritesize(a1,a4,a5)

end sub

sub do_sprite
dim t1,t2,t3 as expr_result 
dim a1,a2,a3 as integer
t3=pop()
t2=pop()
t1=pop()
a1=converttoint(t1) : a2=converttoint(t2) : a3=converttoint(t3)
v.setspritepos(a1,a2,a3)
end sub

sub do_mousex

dim t1 as expr_result
'dim x,y as ulong
'x,y=kbm.mouse_xy()
t1.result_type=result_uint
t1.result.uresult=mousex
push t1
end sub

sub do_mousey

dim t1 as expr_result
'dim x,y as ulong
'x,y=kbm.mouse_xy()
t1.result_type=result_uint
t1.result.uresult=mousey
push t1
end sub

sub do_mousew

dim t1 as expr_result
'dim x,y,z as ulong
'x,y=kbm.mouse_scroll()
t1.result_type=result_int
t1.result.iresult=mousew
push t1
end sub

sub do_mousek

dim t1 as expr_result
'dim k as ulong
'k=kbm.mouse_buttons()
t1.result_type=result_uint
t1.result.uresult=mousek
push t1
end sub
sub do_gettime

dim hi2, lo2 as ulong

dim t1 as expr_result
hi2,lo2=do_gettime2()
t1.result_type=result_uint
t1.result.twowords(0)=lo2
t1.result.twowords(1)=hi2
push t1
end sub

function do_gettime2() as ulong,ulong

dim lo1, hi1 as ulong

const asm 
   getct hi1 wc
   getct lo1
end asm   

return hi1, lo1
end function  
   

'' ----------------------------- Graphics related runtime procedures --------------------------------------

'' ----------------------------- Clear the screen

sub do_cls
cls(ink,paper): plot_color=ink: print
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
dim x,y as integer
t2=pop() 					 
t1=pop()

x=t1.result.iresult
y=t2.result.iresult	
if (t1.result_type=result_int orelse t1.result_type=result_uint) andalso (t2.result_type=result_int orelse t2.result_type=result_uint) then 					  
   plot_x=x			 
   plot_y=y			 
   v.putpixel(plot_x,plot_y,plot_color) 		 
else
  if t1.result_type=result_float then x=round(t1.result.fresult)
  if t2.result_type=result_float then y=round(t2.result.fresult)
  if t1.result_type=result_string then x=val(t1.result.sresult)
  if t2.result_type=result_string then y=val(t2.result.sresult)
  plot_x=x: plot_y=y
  v.putpixel(plot_x,plot_y,plot_color)      
endif
end sub

' --------------------------- Draw a line to point set by plot or previous draw, set a new starting point

sub do_draw
dim t1,t2 as expr_result 
dim x,y as integer

t2=pop()
t1=pop()
x=t1.result.iresult
y=t2.result.iresult
if (t1.result_type=result_int orelse t1.result_type=result_uint) andalso (t2.result_type=result_int orelse t2.result_type=result_uint) then 					  
   v.draw(plot_x,plot_y,x,y,plot_color) 
   plot_x=x
   plot_y=y
else
  if t1.result_type=result_float then x=round(t1.result.fresult)
  if t2.result_type=result_float then y=round(t2.result.fresult)
  if t1.result_type=result_string then x=val(t1.result.fresult)
  if t2.result_type=result_string then y=val(t2.result.fresult)    
  v.draw(plot_x,plot_y,x,y,plot_color) 
  plot_x=x
  plot_y=y 
endif   
end sub

' -------------------------- Draw a filled circle at x,y and radius r

sub do_fcircle
dim t1,t2,t3 as expr_result 

t3=pop()
t2=pop()
t1=pop()
if (t1.result_type=result_int orelse t1.result_type=result_uint) andalso (t2.result_type=result_int orelse t2.result_type=result_uint) andalso (t3.result_type=result_int orelse t3.result_type=result_uint) then
   v.fcircle(t1.result.iresult,t2.result.iresult,t3.result.iresult,plot_color) : return
else
   v.fcircle(converttoint(t1), converttoint(t2), converttoint(t3),plot_color)
endif   
end sub

' -------------------------- Draw an empty circle at x,y and radius r

sub do_circle
dim t1,t2,t3 as expr_result 

t3=pop()
t2=pop()
t1=pop()
if (t1.result_type=result_int orelse t1.result_type=result_uint) andalso (t2.result_type=result_int orelse t2.result_type=result_uint) andalso (t3.result_type=result_int orelse t3.result_type=result_uint) then
   v.circle(t1.result.iresult,t2.result.iresult,t3.result.iresult,plot_color) : return
else
   v.circle(converttoint(t1), converttoint(t2), converttoint(t3),plot_color)
endif   
end sub

' -------------------------- Draw a rectangle

sub do_box
dim t1,t2,t3,t4 as expr_result 

t4=pop()
t3=pop()
t2=pop()
t1=pop()
if (t1.result_type=result_int orelse t1.result_type=result_uint) andalso (t2.result_type=result_int orelse t2.result_type=result_uint) andalso (t3.result_type=result_int orelse t3.result_type=result_uint) andalso (t4.result_type=result_int orelse t4.result_type=result_uint) then
   v.box(t1.result.iresult,t2.result.iresult,t3.result.iresult,t4.result.iresult,plot_color) : return
else
   v.box(converttoint(t1), converttoint(t2), converttoint(t3), converttoint(t4),plot_color)
endif   
end sub

sub do_fill
dim t1,t2,t3,t4 as expr_result 

t4=pop()
t3=pop()
t2=pop()
t1=pop()
if (t1.result_type=result_int orelse t1.result_type=result_uint) andalso (t2.result_type=result_int orelse t2.result_type=result_uint) andalso (t3.result_type=result_int orelse t3.result_type=result_uint) andalso (t4.result_type=result_int orelse t4.result_type=result_uint) then
   v.fill(t1.result.iresult,t2.result.iresult,t3.result.iresult,t4.result.iresult) : return
else
   v.fill(converttoint(t1), converttoint(t2), converttoint(t3), converttoint(t4))
endif   
end sub

' -------------------------- Draw a frane

sub do_frame
dim t1,t2,t3,t4 as expr_result 

t4=pop()
t3=pop()
t2=pop()
t1=pop()
if (t1.result_type=result_int orelse t1.result_type=result_uint) andalso (t2.result_type=result_int orelse t2.result_type=result_uint) andalso (t3.result_type=result_int orelse t3.result_type=result_uint) andalso (t4.result_type=result_int orelse t4.result_type=result_uint) then
   v.frame(t1.result.iresult,t2.result.iresult,t3.result.iresult,t4.result.iresult,plot_color) : return
else
   v.frame(converttoint(t1), converttoint(t2), converttoint(t3), converttoint(t4),plot_color)
endif   
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


sub do_paper
dim t1 as expr_result
t1=pop() 
if t1.result_type=result_float then t1.result.iresult=t1.result.fresult
if t1.result_type=result_string then t1.result.iresult=val(t1.result.sresult)
paper=t1.result.iresult : v.setwritecolors(ink,paper)
end sub

sub do_ink
dim t1 as expr_result
t1=pop() 
if t1.result_type=result_float then t1.result.iresult=t1.result.fresult
if t1.result_type=result_string then t1.result.iresult=val(t1.result.sresult)
ink=t1.result.iresult : v.setwritecolors(ink,paper) : v.setcursorcolor(ink)
end sub


sub do_font
dim t1 as expr_result
t1=pop() 
if t1.result_type=result_float then t1.result.iresult=t1.result.fresult
if t1.result_type=result_string then t1.result.iresult=val(t1.result.sresult)
font=t1.result.iresult : v.setfontfamily(font*4)
end sub

sub do_mode
dim t1 as expr_result
t1=pop() 
if t1.result_type=result_float then t1.result.iresult=t1.result.fresult
if t1.result_type=result_string then t1.result.iresult=val(t1.result.sresult)
select case t1.result.iresult
   case 0: font=1 :ink=154 : paper=147 : v.setfontfamily(4) : v.setwritecolors(ink,paper)
   case 1: font=0 :ink=23 :  paper=0 : v.setfontfamily(0) : v.setwritecolors(ink,paper)
   case 2: font=0 :ink=181 : paper=0 : v.setfontfamily(0) : v.setwritecolors(ink,paper)
   case 3: font=0 :ink=15 :  paper=0 : v.setfontfamily(0) : v.setwritecolors(ink,paper)
end select
v.cls(ink,paper) : v.writeln("") : v.writeln(ver$) : v.writeln(free$) ' todo free has to be computed in the real time
end sub



sub do_pinwrite
dim t1,t2 as expr_result
t1=pop() 'value
t2=pop() ' pin
pinwrite(t2.result.uresult,t1.result.uresult)
end sub

sub do_waitms
dim t1 as expr_result
dim t as integer

t1=pop() 'value

if (t1.result_type=result_int orelse t1.result_type=result_uint) then t=t1.result.iresult else t=converttoint(t1)
if t<0 then return
if t < 5000 then 
  waitms(t)
else
  for i=1 to t/5000
    waitms(t)
  next i
  waitms(t mod 5000)
endif
end sub

sub do_waitvbl
waitvbl
end sub

sub do_waitclock
waitclock
end sub

sub do_dir
dim filename as string
chdir("/sd/bas")       ' set working directory
print "Working directory: "; currentdir$ 

filename=dir$("*", fbDirectory)
while filename <> "" and filename <> nil
  print "[dir] ";filename
  filename = dir$()      ' continue scan

   
end while

filename = dir$("*", fbNormal )  ' start scan for all files and directories
do while filename <> "" and filename <> nil
  print filename
  filename = dir$()      ' continue scan
    if v.getcursory()=34 then    'bug, after first break, cursory is always 35
    print "-----more, press any key";
    do 
    loop while kbm.get_key()<>0
    do
    loop while kbm.get_key()=0
      if keyclick=1 then paula.play(7,@atari_spl,44100,16384,1684) 
    position 0,35: print "                             ";: position 4,35  
  endif  
loop
end sub

sub do_if

dim t1 as expr_result
t1=pop()
' if uresult=0, jump over else
if t1.result.uresult = 0 then 
  for i=lineptr_e to lineptr-1
    t1=compiledline(i)
    if t1.result_type=token_else then lineptr_e=i : return
  next i  
  lineptr_e=lineptr-1
endif
end sub

sub do_else

lineptr_e=lineptr-1
end sub



sub do_nothing
' else, then  itself does nothing.
end sub

sub do_brun

dim t1 as expr_result
dim pos,r,psramptr as integer
dim filename, fullfilename as string

t1=pop() 
if t1.result_type=result_string then
  filename=t1.result.sresult
  if left$(filename,1)="/" then 
    fullfilename=filename
  else
    fullfilename="/sd/bin/"+filename  
  endif  
'  if mid$(filename,2,1)=":" then fullfilename=right$(filename,len(filename-3)) ' todo for future: strip c:/
  open fullfilename for input as #9
  r=geterr() : if r then print "System error ";r;": ";strerror$(r) :close #9 : return
  let pos=1: let r=0 : let psramptr=0
  do
    get #9,pos,block(0),1024,r : pos+=r	
    psram.write(varptr(block(0)),psramptr,1024)	
    psramptr+=r 
					                                         ' move the buffer to the RAM and update RAM position. Todo: this can be done all at once
  loop until r<>1024  orelse psramptr>=$7C000  					         ' do until eof or memory full

' stop all driver cogs except PSRAM

    cpustop(audiocog)
    cpustop(videocog)
    cpustop(usbcog)
    cpustop(housekeeper_cog)

'start loading cog

    let loadingcog=cpu(@loadcog,@pslock) 

' stop itself
    
    cpustop(cpuid())

  endif  
end sub


sub do_mouse

dim t1 as expr_result

t1=pop()

if t1.result.uresult=0 then v.setspritesize(16,0,0) else v.setspritesize(16,32,32)
end sub

sub do_cursor

dim t1 as expr_result

t1=pop()

if t1.result.uresult=0 then  v.setspritesize(17,0,0) else v.setspritesize(17,8,16) 
end sub


sub do_click

dim t1 as expr_result

t1=pop()

if t1.result.uresult=0 then keyclick=0 else keyclick=1
end sub


sub do_beep

dim t1,t2 as expr_result
dim freq as ulong
dim sample(1) as ubyte



t2=pop()
t1=pop()


if (t1.result_type=result_int orelse t1.result_type=result_uint) then freq=t1.result.iresult else freq=converttoint(t1)
sample(0)=127: sample(1)=128
paula.play8(7,varptr(sample),freq*2,16384,2,0)
push t2
do_waitms
paula.stop(7)
end sub

sub do_no_command
printerror(23)
end sub

sub do_negative

dim t1 as expr_result
t1=pop()
if t1.result_type=result_int then 
  t1.result.iresult=-t1.result.iresult
else if t1.result_type=result_uint then 
  t1.result.iresult=-t1.result.uresult : t1.result_type=result_int
else if t1.result_type=result_float then 
  t1.result.fresult=-t1.result.fresult
else 
  t1.result_type=result_error : t1.result.uresult=40
endif
push t1
end sub

'--------------------------- THE END OF THE MAIN PROGRAM ------------------------------------------------------

''----------------------------------------------------------------------------------------------------
''------------------ Initialization procedures -------------------------------------------------------
''----------------------------------------------------------------------------------------------------

''--------------------------- Command function pointers

sub init_commands

for i=0 to 255 : commands(i)=@do_no_command : next i
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
commands(fun_getvar)=@do_getvar
'commands(fun_getuvar)=@do_getuvar
'commands(fun_getfvar)=@do_getfvar
'commands(fun_getsvar)=@do_getsvar
commands(fun_pushu)=@do_push 
commands(fun_pushi)=@do_push  
commands(fun_pushf)=@do_push  
commands(fun_pushs)=@do_push  
commands(fun_assign)=@do_assign
'.commands(fun_assign_i)=@do_assign_i
'.commands(fun_assign_f)=@do_assign_f
'.commands(fun_assign_s)=@do_assign_s  
commands(print_mod_empty)=@do_push
commands(print_mod_comma)=@do_push
commands(print_mod_semicolon)=@do_push
commands(token_cls)=@do_cls
commands(token_new)=@do_new
commands(token_plot)=@do_plot
commands(token_draw)=@do_draw
commands(token_print)=@do_print
commands(token_circle)=@do_circle
commands(token_fcircle)=@do_fcircle
commands(token_box)=@do_box
commands(token_frame)=@do_frame
commands(token_color)=@do_color
commands(token_for)=@do_for
commands(token_next)=@do_next
commands(token_list)=@do_list
commands(token_run)=@do_run
commands(token_error)=@do_error
commands(token_fast_goto)=@do_fast_goto
commands(token_find_goto)=@do_find_goto
commands(token_slow_goto)=@do_slow_goto
commands(fun_converttoint)=@do_converttoint 
commands(token_csave)=@test_csave
commands(token_save)=@do_save
commands(token_load)=@do_load
commands(token_pinwrite)=@do_pinwrite
commands(token_waitms)=@do_waitms
commands(token_waitvbl)=@do_waitvbl
commands(token_if)=@do_if
commands(token_else)=@do_else
commands(token_then)=@do_nothing
commands(token_eq)=@do_eq
commands(token_ge)=@do_ge
commands(token_le)=@do_le
commands(token_gt)=@do_gt
commands(token_lt)=@do_lt
commands(token_ne)=@do_ne
commands(token_rnd)=@do_rnd
commands(token_brun)=@do_brun
commands(token_beep)=@do_beep
commands(token_dir)=@do_dir
commands(token_paper)=@do_paper
commands(token_ink)=@do_ink
commands(token_font)=@do_font
commands(token_mode)=@do_mode
commands(token_find_goto)=@do_find_goto
commands(token_mouse)=@do_mouse
commands(token_gettime)=@do_gettime
commands(token_cursor)=@do_cursor
commands(token_click)=@do_click
commands(token_mousex)=@do_mousex
commands(token_mousey)=@do_mousey
commands(token_mousek)=@do_mousek
commands(token_mousew)=@do_mousew
commands(token_sin)=@do_sin
commands(token_stick)=@do_stick
commands(token_strig)=@do_strig
commands(token_sprite)=@do_sprite
commands(token_defsprite)=@do_defsprite
commands(token_getpixel)=@do_getpixel
commands(token_waitclock)=@do_waitclock
commands(fun_negative)=@do_negative
commands(token_fill)=@do_fill
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
errors$(27)="The program is empty."
errors$(28)="If after if."
errors$(29)="Empty expression."
errors$(30)="String expected."
errors$(31)="Interpreter internal error."
errors$(32)="Expected assign."
errors$(33)="Expected 'to'."
errors$(34)="Expected integer variable."
errors$(35)="Uninitialized variable in 'next', use 'for' before."
errors$(36)="No more slots for 'for'."
errors$(37)="'Next' doesn't match 'for'."
errors$(38)="'Goto' target line not found."
errors$(39)="Bad number of parameters"
errors$(40)="Function undefined for strings"

errors$(41)="Bad parameter."
errors$(42)="Cannot declare an array: the variable exists."
errors$(43)="Expected '('."
errors$(44)="Expected ')' or ','."
errors$(45)="No more than 3 dimensions supported"
errors$(46)="Variable name expected"

end sub
        
sub printerror(err as integer)

v.write("Error " ): v.write(v.inttostr(err)) : v.write(": ")  : v.writeln(errors$(err))
end sub

'' ------------------------------- Hardware start/stop/initialization 

sub startpsram
pscog=psram.startx(0, 1024, 11, 7)
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

videocog=v.start(pin,mbox)

for thecog=0 to 7:psram.setQos(thecog, 80 << 16) :next thecog
psram.setQoS(videocog, $0400f400) 
open SendRecvDevice(@v.putchar, nil, nil) as #0
return videocog
waitms(100)

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
mouse  file "mouse.def"
end asm

'' ------------------------------- the loader cog for BRUN

		asm shared

             	org
loadcog      	cogid   t11              		' get a cogid
                mul     t11, #12                        ' compute the offset to PSRAM mailbox 
                add     mailbox, t11                     ' add offset to find this COG's mailbox

                mov     psramaddr,#0

p101            mov     buf1,psramaddr			' psramaddr=hubaddr
                mov     buf2,##16384			' loading size
                mov     cmd,psramaddr                   ' set the address for reading
                setnib  cmd, #%1011, #7                 ' attach the command - read burst
                setq    #2			 	' write 3 longs to the mailbox
                wrlong  cmd, mailbox			' read the PSRAM
p102            rdlong  cmd, mailbox                	' poll mailbox for result
                tjs     cmd, #p102                 	' retry until valid 

                add 	psramaddr,##16384
		cmp 	psramaddr,##$7C000 wcz
	if_lt	jmp 	#p101				' loop until full hub loaded

                
                cogstop #7				' stop psram driver
    
                cogid 	t11				' get id
                coginit #0,#0				' start the new program
                cogstop t11				' stop the loader

t11 		long 	0
mailbox 	long 	$7FF00
psramaddr 	long 	0
pslockval	long 	0
cmd             long    0
buf1            long    0
buf2            long    16384

		end asm
		
'----- The end ---------------------------------------------------------------------------------------------------
