#include "retromachine.bi"
#include "windows.bas"
const hubset338=%1_111011__11_1111_0111__1111_1011 '338_666_667 =30*44100 
const hubset354=%1_110000__11_0110_1100__1111_0111 '354693878,   %1_110000__11_0110_1100__1111_1011
const HEAPSIZE=16384
dim s(512) as ulong
dim s2(512) as ulong

let c1=1: let c2=0
'startmachine
startpsram
startvideo
hubset(hubset338)
v.cls(200,0)

for i=0 to 15: v.box(100*i,0,100*i+99,500,8+i*16) : next i
position 0,40: print hex$(v.buf_ptr)


'psram.gfxCopyImage(v.buf_ptr+100+192000, 1920, v.buf_ptr, 1920, 300, 300, $60000, $70000)
' pub blit(f,x1a,y1a,x2a,y2a,s1,t,x1b,y1b,s2) | y

let t1=getct():v.psblit(v.buf_ptr,000,1000,1920,180,1920, v.buf_ptr,0,0,1920) : t1=getct()-t1 : print "full blit:",t1/336


let t1=getct():for i=0 to 1079: for j=0 to 1919: next j: next i: : t1=getct()-t1 : print t1/336
let t1=getct()

cpu asm
  mov 495,##1080
p2  mov 494,##1920
p1 djnz 494,#p1
   djnz 495,#p2
 end asm  



t1=getct()-t1 : print t1/336
waitms(5000): waitms(5000) : waitms(5000) : waitms(5000)
pinlo(38): pinlo(39)
'list1
maketestdl
let delta=1
let olddl=v.dl_ptr
v.dl_ptr=addr(dl1(0))
dim ccc,x1,x2,y1,y2,r as ulong
v.cls(200,0)
let cog=cpu(movesprite,@s)
let cog2=cpu(windowtest,@s2)
v.setfontfamily(0)
'hubset(hubset354)
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
dolist
print "list: ";ttm/36

for i=0 to 15
  for j=0 to 15
    print rtable(i,j),
  next j
  print
next i    
waitms(5000): waitms(5000) : waitms(5000) : waitms(5000)
'psram.gfxCopyImage(dstAddr, dstPitch, srcAddr, srcPitch, byteWidth, height, hubBuffer, listPtr)



'for i=0 to vcount: print xtable(i),ytable(i): next i
' for i=0 to rectnum-1: print rectangles(i).x1,rectangles(i).y1,rectangles(i).x2,rectangles(i).y2, rectangles(i).handle : next i


'windows(test1).scrollup
for i=0 to 4: waitms(5000): next i
'do:loop

let loopcnt=1
do
  loopcnt+=1
  windows(test1).writeln("Window test "+str$(loopcnt))
  for i=1 to 500: print i: next i : v.write("501") : waitms(5000)

  for i=0 to 10000
    ccc=getrnd() and 255
    x1=getrnd() mod 240 
    y1=getrnd() mod 66                                
    position 2*x1,y1: v.setwritecolors(ccc,0): v.write("Testing PSRAM 8bpp 1920x1080 VGA mode fast write")
  next i

  for i=0 to 1000
    ccc=getrnd() and 255
    x1=getrnd() mod 1920
    y1=getrnd() mod 1080
    v.outtextxycg(x1,y1,"Testing PSRAM 8bpp 1920x1080 VGA mode slow write",ccc,0)
  next i
  for i = 0 to 5000
    ccc=getrnd() and 255
    x1=getrnd() mod 1920
    x2=getrnd() mod 1920
    y1=getrnd() mod 1080
    y2=getrnd() mod 1080
    v.draw(x1,y1,x2,y2,ccc)
  next i 

  for i = 0 to 5000
    x1=getrnd() mod 1920
    y1=getrnd() mod 1080
    r=getrnd() mod 100
    ccc=getrnd() and 255
    v.fcircle(x1,y1,r,ccc)   
  next i  

  for i = 0 to 5000
    ccc=getrnd() and 255
    x1=getrnd() mod 1920
    x2=getrnd() mod 1920
    y1=getrnd() mod 1080
    y2=getrnd() mod 1080
    v.frame(x1,y1,x2,y2,ccc)
  next i  
  
  for i = 0 to 5000
    x1=getrnd() mod 1920
    y1=getrnd() mod 1080
    r=getrnd() mod 100
    ccc=getrnd() and 255
    v.circle(x1,y1,r,ccc) 
  next i  
  
  for i = 0 to 10000
    ccc=getrnd() and 255
    x1=getrnd() mod 1920
    x2=getrnd() mod 200
    y1=getrnd() mod 1080
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


let xpos1=10
let ypos1=10
let xpos2=61
let ypos2=60
let xpos3=112
let ypos3=110
let xpos4=163
let ypos4=160
let xpos5=214
let ypos5=210
let xpos6=265
let ypos6=260
let xpos7=316
let ypos7=310
let xpos8=367
let ypos8=360
let xpos9=19
let ypos9=9
let xpos10=61
let ypos10=60
let xpos11=112
let ypos11=110
let xpos12=163
let ypos12=160
let xpos13=214
let ypos13=210
let xpos14=265
let ypos14=260
let xpos15=316
let ypos15=310
let xpos16=367
let ypos16=360

let xdelta1=1
let ydelta1=1
let xdelta2=2
let ydelta2=2
let xdelta3=3
let ydelta3=3
let xdelta4=4
let ydelta4=4
let xdelta5=5
let ydelta5=5
let xdelta6=6
let ydelta6=6
let xdelta7=7
let ydelta7=7
let xdelta8=8
let ydelta8=8

let xdelta16=-1
let ydelta16=-1
let xdelta15=-2
let ydelta15=-2
let xdelta14=-3
let ydelta14=-3
let xdelta13=-4
let ydelta13=-4
let xdelta12=-5
let ydelta12=-5
let xdelta11=-6
let ydelta11=-6
let xdelta10=-7
let ydelta10=-7
let xdelta9=-8
let ydelta9=-8
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


if xpos1>=1920-32 then xdelta1=-1
if ypos1>=1080-32 then ydelta1=-1
if ypos1=0 then ydelta1=1
if xpos1=0 then xdelta1=1

if xpos2>=1920-32 then xdelta2=-1
if ypos2>=1080-32 then ydelta2=-1
if ypos2<=0 then ydelta2=2
if xpos2<=0 then xdelta2=2

if xpos3>=1920-32 then xdelta3=-3
if ypos3>=1080-32 then ydelta3=-3
if ypos3<=0 then ydelta3=2
if xpos3<=0 then xdelta3=2

if xpos4>=1920-32 then xdelta4=-2
if ypos4>=1080-32 then ydelta4=-2
if ypos4<=0 then ydelta4=2
if xpos4<=0 then xdelta4=2

if xpos5>=1920-32 then xdelta5=-3
if ypos5>=1080-32 then ydelta5=-3
if ypos5<=0 then ydelta5=4
if xpos5<=0 then xdelta5=4

if xpos6>=1920-32 then xdelta6=-4
if ypos6>=1080-32 then ydelta6=-4
if ypos6<=0 then ydelta6=3
if xpos6<=0 then xdelta6=3

if xpos7>=1920-32 then xdelta7=-4
if ypos7>=1080-32 then ydelta7=-4
if ypos7<=0 then ydelta7=4
if xpos7<=0 then xdelta7=4

if xpos8>=1920-32 then xdelta8=-4
if ypos8>=1080-32 then ydelta8=-4
if ypos8<=0 then ydelta8=5
if xpos8<=0 then xdelta8=5


if xpos16>=1920-32 then xdelta16=-1
if ypos16>=1080-32 then ydelta16=-1
if ypos16=0 then ydelta16=1
if xpos16=0 then xdelta16=1


if xpos15>=1920-32 then xdelta15=-1
if ypos15>=1080-32 then ydelta15=-1
if ypos15<=0 then ydelta15=2
if xpos15<=0 then xdelta15=2


if xpos14>=1920-32 then xdelta14=-3
if ypos14>=1080-32 then ydelta14=-3
if ypos14<=0 then ydelta14=2
if xpos14<=0 then xdelta14=2

if xpos13>=1920-32 then xdelta13=-2
if ypos13>=1080-32 then ydelta13=-2
if ypos13<=0 then ydelta13=2
if xpos13<=0 then xdelta13=2

if xpos12>=1920-32 then xdelta12=-3
if ypos12>=1080-32 then ydelta12=-3
if ypos12<=0 then ydelta12=4
if xpos12<=0 then xdelta12=4

if xpos11>=1920-32 then xdelta11=-3
if ypos11>=1080-32 then ydelta11=-3
if ypos11<=0 then ydelta11=3
if xpos11<=0 then xdelta11=3

if xpos10>=1920-32 then xdelta10=-4
if ypos10>=1080-32 then ydelta10=-4
if ypos10<=0 then ydelta10=5
if xpos10<=0 then xdelta10=5

if xpos9>=1920-32 then xdelta9=-4
if ypos9>=1080-32 then ydelta9=-4
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
let framenum=(framenum+delta) mod 1600
if framenum=0 then delta=1
if framenum=1599 then delta=-1
makelist01(framenum)
loop
end sub

dim list1(1080,11) as ulong

sub makelist01(pos)

for i=0 to 1079: testlist01(i,pos): next i
end sub

sub testlist01(linenum,pos)

list1(linenum,0)=$B0000000+v.buf_ptr+1920*linenum 					'read from $100000
list1(linenum,1)=$80000-16384-7680+1920*(linenum mod 4)
list1(linenum,2)=pos+1
list1(linenum,3)=addr(list1(linenum,4))
if linenum<0+framenum/2 orelse linenum>199+framenum/2 then list1(linenum,3)=0: list1(linenum,2)=1920
list1(linenum,4)=$B0_60_0000+320*(linenum-framenum/2) 					'read from $100000
list1(linenum,5)=$80000-16384-7680+1920*(linenum mod 4)+pos
list1(linenum,6)=320
list1(linenum,7)=addr(list1(linenum,8))
list1(linenum,8)=$B0000000+v.buf_ptr+1920*linenum +320+pos					'read from $100000
list1(linenum,9)=$80000-16384-7680+1920*(linenum mod 4)+320+pos
list1(linenum,10)=1920-320-pos
list1(linenum,11)=0
end sub



dim dl1(1081)

sub maketestdl
for i=0 to 1079
  dl1(i)=addr(list1(i,0)) shl 4
  testlist01(i,256)
next i
dl1(1080)=0: 
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
