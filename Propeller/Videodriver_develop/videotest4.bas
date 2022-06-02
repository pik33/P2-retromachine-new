#include "retromachine.bi"
#include "windows.bas"
const hubset338=%1_111011__11_1111_0111__1111_1011 '338_666_667 =30*44100 
const hubset354=%1_110000__11_0110_1100__1111_1011 '354693878,   %1_110000__11_0110_1100__1111_1011
const HEAPSIZE=16384
dim s(512) as ulong
dim s2(512) as ulong

let c1=1: let c2=0
'startmachine
startpsram
startvideo


pinlo(38): pinlo(39)
list1
maketestdl
let delta=1
let olddl=v.dl_ptr
v.dl_ptr=addr(dl1(0))
dim ccc,x1,x2,y1,y2,r as ulong
v.cls(200,0)
let cog=cpu(movesprite,@s)
let cog2=cpu(windowtest,@s2)
v.setfontfamily(0)
''hubset(hubset354)
initwindows

let background=createwindow(1024,576,0, $800000)
move(background,0,0,1024,576,0,0)
let test1=createwindow(320,200,0,$600000)
move(test1,50,50,50,50,0,0)
let test2=createwindow(320,200,0,$500000)
move(test2,150,60,50,50,0,0)
let test3=createwindow(320,200,0,$400000)
move(test3,250,70,50,50,0,0)
let test4=createwindow(320,200,0,$380000)
move(test4,380,90,100,100,0,0)
let test5=createwindow(320,200,0,$300000)
move(test5,450,400,32,20,0,0)
let test6=createwindow(320,200,0,$280000)
move(test6,550,440,44,40,0,0)
let test7=createwindow(320,200,0,$200000)
move(test7,650,500,320,200,0,0)
windows(test7).show
windows(test6).show
windows(test5).show
windows(test4).show
windows(test3).show
windows(test2).show
windows(test1).writeln("Window test")
windows(test1).show
windows(background).show
getrects
'print vcount
print rectnum
print ttm/336
print kwas
'for i=0 to vcount: print xtable(i),ytable(i): next i
' for i=0 to rectnum-1: print rectangles(i).x1,rectangles(i).y1,rectangles(i).x2,rectangles(i).y2, rectangles(i).handle : next i


'windows(test1).scrollup
for i=0 to 10: waitms(5000): next i
'do:loop

let loopcnt=1
do
  loopcnt+=1
  windows(test1).writeln("Window test "+str$(loopcnt))
  for i=1 to 500: print i: next i : v.write("501") : waitms(5000)

  for i=0 to 10000
    ccc=getrnd() and 255
    x1=getrnd() mod 128 
    y1=getrnd() mod 35                                
    position 2*x1,y1: v.setwritecolors(ccc,0): v.write("Testing PSRAM 8bpp 1024x576 HDMI mode 00")
  next i

  for i=0 to 1000
    ccc=getrnd() and 255
    x1=getrnd() mod 1024
    y1=getrnd() mod 576
    v.outtextxycg(x1,y1,"Testing PSRAM 8bpp 1024x576 HDMI mode 00",ccc,0)
  next i
  for i = 0 to 5000
    ccc=getrnd() and 255
    x1=getrnd() mod 1024
    x2=getrnd() mod 1024
    y1=getrnd() mod 576
    y2=getrnd() mod 576
    v.draw(x1,y1,x2,y2,ccc)
  next i 

  for i = 0 to 5000
    x1=getrnd() mod 1024
    y1=getrnd() mod 576
    r=getrnd() mod 100
    ccc=getrnd() and 255
    v.fcircle(x1,y1,r,ccc)   
  next i  

  for i = 0 to 5000
    ccc=getrnd() and 255
    x1=getrnd() mod 1024
    x2=getrnd() mod 1024
    y1=getrnd() mod 576
    y2=getrnd() mod 576
    v.frame(x1,y1,x2,y2,ccc)
  next i  
  
  for i = 0 to 5000
    x1=getrnd() mod 1024
    y1=getrnd() mod 576
    r=getrnd() mod 100
    ccc=getrnd() and 255
    v.circle(x1,y1,r,ccc) 
  next i  
  
  for i = 0 to 10000
    ccc=getrnd() and 255
    x1=getrnd() mod 1024
    x2=getrnd() mod 200
    y1=getrnd() mod 576
    y2=getrnd() mod 200
    v.box(x1,y1,x1+x2,y1+y2,ccc)  
  next i      
  
loop

sub movesprite

v.spr1ptr=@balls
v.spr2ptr=@balls2
v.spr3ptr=@balls3
v.spr4ptr=@balls4
v.spr5ptr=@balls5
v.spr6ptr=@balls6
v.spr7ptr=@balls7
v.spr8ptr=@balls8
v.spr9ptr=@balls
v.spr10ptr=@balls2
v.spr11ptr=@balls3
v.spr12ptr=@balls4
v.spr13ptr=@balls5
v.spr14ptr=@balls6
v.spr15ptr=@balls7
v.spr16ptr=@balls8

v.spr1h=32
v.spr1w=32
v.spr2h=32
v.spr2w=32
v.spr3h=32
v.spr3w=32
v.spr4h=32
v.spr4w=32
v.spr5h=32
v.spr5w=32
v.spr6h=32
v.spr6w=32
v.spr7h=32
v.spr7w=32
v.spr8h=32
v.spr8w=32
v.spr9h=32
v.spr9w=32
v.spr10h=32
v.spr10w=32
v.spr11h=32
v.spr11w=32
v.spr12h=32
v.spr12w=32
v.spr13h=32
v.spr13w=32
v.spr14h=32
v.spr14w=32
v.spr15h=32
v.spr15w=32
v.spr16h=32
v.spr16w=32


var xpos1=10
var ypos1=10
var xpos2=61
var ypos2=60
var xpos3=112
var ypos3=110
var xpos4=163
var ypos4=160
var xpos5=214
var ypos5=210
var xpos6=265
var ypos6=260
var xpos7=316
var ypos7=310
var xpos8=367
var ypos8=360
var xpos9=19
var ypos9=9
var xpos10=61
var ypos10=60
var xpos11=112
var ypos11=110
var xpos12=163
var ypos12=160
var xpos13=214
var ypos13=210
var xpos14=265
var ypos14=260
var xpos15=316
var ypos15=310
var xpos16=367
var ypos16=360

var xdelta1=1
var ydelta1=1
var xdelta2=2
var ydelta2=2
var xdelta3=3
var ydelta3=3
var xdelta4=4
var ydelta4=4
var xdelta5=5
var ydelta5=5
var xdelta6=6
var ydelta6=6
var xdelta7=7
var ydelta7=7
var xdelta8=8
var ydelta8=8

var xdelta16=-1
var ydelta16=-1
var xdelta15=-2
var ydelta15=-2
var xdelta14=-3
var ydelta14=-3
var xdelta13=-4
var ydelta13=-4
var xdelta12=-5
var ydelta12=-5
var xdelta11=-6
var ydelta11=-6
var xdelta10=-7
var ydelta10=-7
var xdelta9=-8
var ydelta9=-8
let frames=0

do

do:loop until v.vblank=1
let frames=(frames+1) mod 16

v.spr1ptr=@balls+1024*frames
v.spr2ptr=@balls2+1024*frames
v.spr3ptr=@balls3+1024*frames
v.spr4ptr=@balls4+1024*frames
v.spr5ptr=@balls5+1024*frames
v.spr6ptr=@balls6+1024*frames
v.spr7ptr=@balls7+1024*frames
v.spr8ptr=@balls8+1024*frames
v.spr9ptr=@balls9+1024*frames
v.spr10ptr=@balls10+1024*frames
v.spr11ptr=@balls11+1024*frames
v.spr12ptr=@balls12+1024*frames
v.spr13ptr=@balls13+1024*frames
v.spr14ptr=@balls14+1024*frames
v.spr15ptr=@balls15+1024*frames
v.spr16ptr=@balls16+1024*frames 



xpos1+=xdelta1
ypos1+=ydelta1
xpos2+=xdelta2
ypos2+=ydelta2
xpos3+=xdelta3
ypos3+=ydelta3
xpos4+=xdelta4
ypos4+=ydelta4
xpos5+=xdelta5
ypos5+=ydelta5
xpos6+=xdelta6
ypos6+=ydelta6
xpos7+=xdelta7
ypos7+=ydelta7
xpos8+=xdelta8
ypos8+=ydelta8
xpos9+=xdelta9
ypos9+=ydelta9
xpos10+=xdelta10
ypos10+=ydelta10
xpos11+=xdelta11
ypos11+=ydelta11
xpos12+=xdelta12
ypos12+=ydelta12
xpos13+=xdelta13
ypos13+=ydelta13
xpos14+=xdelta14
ypos14+=ydelta14
xpos15+=xdelta15
ypos15+=ydelta15
xpos16+=xdelta16
ypos16+=ydelta16


if xpos1>=1024-32 then xdelta1=-1
if ypos1>=576-32 then ydelta1=-1
if ypos1=0 then ydelta1=1
if xpos1=0 then xdelta1=1

if xpos2>=1024-32 then xdelta2=-1
if ypos2>=576-32 then ydelta2=-1
if ypos2<=0 then ydelta2=2
if xpos2<=0 then xdelta2=2

if xpos3>=1024-32 then xdelta3=-3
if ypos3>=576-32 then ydelta3=-3
if ypos3<=0 then ydelta3=2
if xpos3<=0 then xdelta3=2

if xpos4>=1024-32 then xdelta4=-2
if ypos4>=576-32 then ydelta4=-2
if ypos4<=0 then ydelta4=2
if xpos4<=0 then xdelta4=2

if xpos5>=1024-32 then xdelta5=-3
if ypos5>=576-32 then ydelta5=-3
if ypos5<=0 then ydelta5=4
if xpos5<=0 then xdelta5=4

if xpos6>=1024-32 then xdelta6=-4
if ypos6>=576-32 then ydelta6=-4
if ypos6<=0 then ydelta6=3
if xpos6<=0 then xdelta6=3

if xpos7>=1024-32 then xdelta7=-4
if ypos7>=576-32 then ydelta7=-4
if ypos7<=0 then ydelta7=4
if xpos7<=0 then xdelta7=4

if xpos8>=1024-32 then xdelta8=-4
if ypos8>=576-32 then ydelta8=-4
if ypos8<=0 then ydelta8=5
if xpos8<=0 then xdelta8=5


if xpos16>=1024-32 then xdelta16=-1
if ypos16>=576-32 then ydelta16=-1
if ypos16=0 then ydelta16=1
if xpos16=0 then xdelta16=1


if xpos15>=1024-32 then xdelta15=-1
if ypos15>=576-32 then ydelta15=-1
if ypos15<=0 then ydelta15=2
if xpos15<=0 then xdelta15=2


if xpos14>=1024-32 then xdelta14=-3
if ypos14>=576-32 then ydelta14=-3
if ypos14<=0 then ydelta14=2
if xpos14<=0 then xdelta14=2

if xpos13>=1024-32 then xdelta13=-2
if ypos13>=576-32 then ydelta13=-2
if ypos13<=0 then ydelta13=2
if xpos13<=0 then xdelta13=2

if xpos12>=1024-32 then xdelta12=-3
if ypos12>=576-32 then ydelta12=-3
if ypos12<=0 then ydelta12=4
if xpos12<=0 then xdelta12=4

if xpos11>=1024-32 then xdelta11=-3
if ypos11>=576-32 then ydelta11=-3
if ypos11<=0 then ydelta11=3
if xpos11<=0 then xdelta11=3

if xpos10>=1024-32 then xdelta10=-4
if ypos10>=576-32 then ydelta10=-4
if ypos10<=0 then ydelta10=5
if xpos10<=0 then xdelta10=5

if xpos9>=1024-32 then xdelta9=-4
if ypos9>=576-32 then ydelta9=-4
if ypos9<=0 then ydelta9=4
if xpos9<=0 then xdelta9=4


if xpos1>=0 then v.spr1x=xpos1
if xpos2>=0 then v.spr2x=xpos2
if xpos3>=0 then v.spr3x=xpos3
if xpos4>=0 then v.spr4x=xpos4
if xpos5>=0 then v.spr5x=xpos5
if xpos6>=0 then v.spr6x=xpos6
if xpos7>=0 then v.spr7x=xpos7
if xpos8>=0 then v.spr8x=xpos8

if xpos9>=0 then v.spr9x=xpos9
if xpos10>=0 then v.spr10x=xpos10
if xpos11>=0 then v.spr11x=xpos11
if xpos12>=0 then v.spr12x=xpos12
if xpos13>=0 then v.spr13x=xpos13
if xpos14>=0 then v.spr14x=xpos14
if xpos15>=0 then v.spr15x=xpos15
if xpos16>=0 then v.spr16x=xpos16

if ypos1>=0 then v.spr1y=ypos1
if ypos2>=0 then v.spr2y=ypos2
if ypos3>=0 then v.spr3y=ypos3
if ypos4>=0 then v.spr4y=ypos4
if ypos5>=0 then v.spr5y=ypos5
if ypos6>=0 then v.spr6y=ypos6
if ypos7>=0 then v.spr7y=ypos7
if ypos8>=0 then v.spr8y=ypos8


if ypos9>=0 then v.spr9y=ypos9
if ypos10>=0 then v.spr10y=ypos10
if ypos11>=0 then v.spr11y=ypos11
if ypos12>=0 then v.spr12y=ypos12
if ypos13>=0 then v.spr13y=ypos13
if ypos14>=0 then v.spr14y=ypos14
if ypos15>=0 then v.spr15y=ypos15
if ypos16>=0 then v.spr16y=ypos16





do:loop until v.vblank=0
let framenum=(framenum+delta) mod 768
if framenum=0 then delta=1
if framenum=703 then delta=-1
makelist01(framenum)
'windows(test1).cog=cpuid()
'windows(test1).mailbox=mbox+12*cpuid()
'windows(test1).writeln(decuns$(framenum))
    let xx1=getrnd() mod 320
    let yy1=getrnd() mod 200
    let rr=getrnd() mod 50
    let cccc=getrnd() and 255
'    windows(test1).fcircle(xx1,yy1,rr,cccc)
loop
end sub

dim list1(575,11) as ulong

sub makelist01(pos)

for i=0 to 575: testlist01(i,pos): next i
end sub

sub testlist01(linenum,pos)

list1(linenum,0)=$B0000000+v.buf_ptr+1024*linenum 					'read from $100000
list1(linenum,1)=$80000-16384-4096+1024*(linenum mod 4)
list1(linenum,2)=pos+1
list1(linenum,3)=addr(list1(linenum,4))
if linenum<0+framenum/2 orelse linenum>199+framenum/2 then list1(linenum,3)=0: list1(linenum,2)=1024
list1(linenum,4)=$B0_60_0000+320*(linenum-framenum/2) 					'read from $100000
list1(linenum,5)=$80000-16384-4096+1024*(linenum mod 4)+pos
list1(linenum,6)=320
list1(linenum,7)=addr(list1(linenum,8))
list1(linenum,8)=$B0000000+v.buf_ptr+1024*linenum +320+pos					'read from $100000
list1(linenum,9)=$80000-16384-4096+1024*(linenum mod 4)+320+pos
list1(linenum,10)=1024-320-pos
list1(linenum,11)=0
end sub



dim dl1(577)

sub maketestdl
for i=0 to 575
  dl1(i)=addr(list1(i,0)) shl 4
  testlist01(i,256)
next i
dl1(576)=0: dl1(577)=0
for i=0 to 255: pspoke $100000+i,i : next i
end sub


sub windowtest

windows(test1).cog=cpuid()
windows(test1).winmailbox=mbox+12*cpuid()
waitms(5000)
do
  for ii=1 to 500: windows(test1).writeln(str$(ii)): next ii : windows(test1).write("501") 

  for ii=0 to 10000
    let cccc=getrnd() and 255
    let xx1=getrnd() mod 40 
    let yy1=getrnd() mod 12                               
    windows(test1).setcursorpos(2*xx1,yy1) : windows(test1).setwritecolors(cccc,0): windows(test1).write("Testing PSRAM window")
  next ii

  for ii=0 to 1000
    cccc=getrnd() and 255
    xx1=getrnd() mod 320
    yy1=getrnd() mod 200
    windows(test1).outtextxycg(xx1,yy1,"Testing PSRAM window outtextxycg",cccc,0)
  next ii
  
  for ii = 0 to 5000
    cccc=getrnd() and 255
    xx1=getrnd() mod 320
    let xx2=getrnd() mod 320
    yy1=getrnd() mod 200
    let yy2=getrnd() mod 200
    windows(test1).draw(xx1,yy1,xx2,yy2,cccc)
  next ii 

  for ii = 0 to 5000
    xx1=getrnd() mod 320
    yy1=getrnd() mod 200
    rr=getrnd() mod 50
    cccc=getrnd() and 255
    windows(test1).fcircle(xx1,yy1,rr,cccc)   
  next ii  

  for ii = 0 to 5000
    cccc=getrnd() and 255
    xx1=getrnd() mod 320
    xx2=getrnd() mod 320
    yy1=getrnd() mod 200
    yy2=getrnd() mod 200
    windows(test1).frame(xx1,yy1,xx2,yy2,cccc)
  next ii  
  
  for ii = 0 to 5000
    xx1=getrnd() mod 320
    yy1=getrnd() mod 200
    let rr=getrnd() mod 50
    cccc=getrnd() and 255
    windows(test1).circle(xx1,yy1,rr,cccc) 
  next ii  
  
  for ii = 0 to 10000
    cccc=getrnd() and 255
    xx1=getrnd() mod 320
    xx2=getrnd() mod 100
    yy1=getrnd() mod 100
    yy2=getrnd() mod 100
    windows(test1).box(xx1,yy1,xx1+xx2,yy1+yy2,cccc)  
  next ii      
  
loop
end sub


asm shared

balls file "balls00def"
balls2 file "balls01def"
balls3 file "balls02def"
balls4 file "balls03def"
balls5 file "balls04def"
balls6 file "balls05def"
balls7 file "balls06def"
balls8 file "balls07def"
balls9 file "balls08def"
balls10 file "balls09def"
balls11 file "balls0Adef"
balls12 file "balls0Bdef"
balls13 file "balls0Cdef"
balls14 file "balls0Ddef"
balls15 file "balls0Edef"
balls16 file "balls0Fdef"

end asm
