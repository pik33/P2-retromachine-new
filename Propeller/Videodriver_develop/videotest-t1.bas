const _clkfreq=336_956_522

mount "/sd", _vfs_open_sdcard() 

dim v as class using "ht009.spin2"
dim psram as class using "psram.spin2"

sub cls(fg=154,bg=147) 
v.cls(fg,bg)
end sub

sub position(x,y)
v.setcursorpos(x,y)
end sub

dim videocog as ulong
dim mbox as ulong
dim ccc,x1,y1,x2,y2,r as ulong

psram.startx(0, 0, 11, -1)
mbox=psram.getMailbox(0)

let pin=0 : videocog=v.start(pin,mbox)
for thecog=0 to 7:psram.setQos(thecog, 112 << 16) :next thecog
psram.setQoS(videocog, $7FFFf400) 
open SendRecvDevice(@v.putchar, nil, nil) as #0
cls(15,0)

v.spr1ptr=@mouse

v.spr1x=-5
v.spr1y=10
v.spr1w=32
v.spr1h=32
v.spr2ptr=@balls

';v.spr2x=200
';v.spr2y=200
'v.spr2w=32
'v.spr2h=32

open "/sd/screen.raw" for input as #8
let pos=1
dim pixel as ulong
for i=0 to 575
  for j=0 to 1023
    get #8,pos,pixel,1
    pixel=((pixel and $FF000000)>>16)+(pixel and $00FF0000)+((pixel and $0000FF00)<<16)
    v.putpixel(j,i,pixel)
    pos+=4
  next j
next i
close #8 

v.blit($4000_0000+v.buf_ptr,0,0,1023,575,4096,$4100_0000,0,0,4096) 
v.outtextxycf(0,0,"1024x576 True Color HDMI driver",15)   
waitms(5000)    
waitms(5000)    

do

  for i=1 to 200: v.setwritecolors (getrnd()<<8, 0) : print i: next i : waitms(1000)
  
  v.blit($4100_0000,0,0,1023,575,4096,$4000_0000+v.buf_ptr,0,0,4096) : waitms(1000)
  
  for i=0 to 5000
    ccc=getrnd() shl 8
    x1=getrnd() mod 128 
    y1=getrnd() mod 36                                
    position 2*x1,y1
    v.setwritecolors(ccc,0)
    v.write("Testing PSRAM 32bpp 1024x576 HDMI mode fast write")
  next i
  waitms(1000)
  
  v.blit($4100_0000,0,0,1023,575,4096,$4000_0000+v.buf_ptr,0,0,4096) : waitms(1000)

  for i=0 to 500
    ccc=getrnd() <<8
    x1=getrnd() mod 1024
    y1=getrnd() mod 560
    v.outtextxycg(x1,y1,"Testing PSRAM 32bpp 1024x576 HDMI1920x1080 VGA mode slow write",ccc,0)
  next i
  waitms(1000)
  
  v.blit($4100_0000,0,0,1023,575,4096,$4000_0000+v.buf_ptr,0,0,4096) : waitms(1000)

  for i = 0 to 5000
    ccc=getrnd() shl 8
    x1=getrnd() mod 1024
    x2=getrnd() mod 1024
    y1=getrnd() mod 576
    y2=getrnd() mod 576
    v.draw(x1,y1,x2,y2,ccc)
    next i 
  waitms(1000) 
 
  v.blit($4100_0000,0,0,1023,575,4096,$4000_0000+v.buf_ptr,0,0,4096) : waitms(1000)

  for i = 0 to 5000
    x1=getrnd() mod 1024
    y1=getrnd() mod 576
    r=getrnd() mod 100
    ccc=getrnd() shl 8
    v.fcircle(x1,y1,r,ccc)   
  next i  
  waitms(1000)
 
  v.blit($4100_0000,0,0,1023,575,4096,$4000_0000+v.buf_ptr,0,0,4096) : waitms(1000) 

  for i = 0 to 5000
    x1=getrnd() mod 1024
    y1=getrnd() mod 576
    r=getrnd() mod 100
    ccc=getrnd() shl 8
    v.circle(x1,y1,r,ccc)   
  next i  
  waitms(1000)

  v.blit($4100_0000,0,0,1023,575,4096,$4000_0000+v.buf_ptr,0,0,4096) : waitms(1000) 

  for i = 0 to 5000
    ccc=getrnd() shl 8
    x1=getrnd() mod 1024
    x2=getrnd() mod 1024
    y1=getrnd() mod 576
    y2=getrnd() mod 576
    v.frame(x1,y1,x2,y2,ccc)
  next i  
  waitms(1000)
  
  v.blit($4100_0000,0,0,1023,575,4096,$4000_0000+v.buf_ptr,0,0,4096) : waitms(1000) 
 
  for i = 0 to 5000
    ccc=getrnd() shl 8
    x1=getrnd() mod 1024
    x2=getrnd() mod 200
    y1=getrnd() mod 576
    y2=getrnd() mod 200
    v.box(x1,y1,x1+x2,y1+y2,ccc)   
  next i    
  waitms(1000)
  v.blit($4100_0000,0,0,1023,575,4096,$4000_0000+v.buf_ptr,0,0,4096) : waitms(1000)

loop

asm shared
mouse file "mouse32.def"
balls file "balls32.def"
end asm
