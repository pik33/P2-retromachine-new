const clkfreq=336956522
dim v as class using "ht009.spin2"
'dim v as class using "hg008.spin2"
dim psram as class using "psram.spin2"
#define addr varptr
dim videocog as integer
dim mbox as ulong
dim ccc,x1,y1 as ulong
sub position(x,y)
v.setcursorpos(x,y)
end sub

sub startpsram
psram.startx(0, 0, 11, -1)
mbox=psram.getMailbox(0)
end sub

sub cls(fg=154,bg=147) 
v.cls(v.getcolor(fg),v.getcolor(bg))
'v.cls((fg),(bg))

end sub

function startvideo(mode=64, pin=0, mb=0) 'todo return a cog#
dim videocog as ulong
videocog=v.start(pin,mbox)
v.setbordercolor(0,0,0)
for thecog=0 to 7:psram.setQos(thecog, 112 << 16) :next thecog
psram.setQoS(videocog, $7FFFf400) 
open SendRecvDevice(@v.putchar, nil, nil) as #0
return videocog
end function

startpsram
startvideo
  _setbaud(921600)
  print clkfreq
cls  
print hex$(v.s_buf_ptr,8)

print "color",hex$(v.getcolor(15))
print v.s_buflen
print "kwas"
waitms(5000)
do
  for i=0 to 10000
     ccc=getrnd() shl 8
    x1=getrnd() mod 128 
    y1=getrnd() mod 36                                
    position 2*x1,y1
     v.setwritecolors(ccc,0)
      v.write("Testing PSRAM 32bpp 1024x576 HDMI mode fast write")
  next i

  for i=0 to 1000
   ccc=getrnd() <<8
    x1=getrnd() mod 1024
    y1=getrnd() mod 540
    v.outtextxycg(x1,y1,"Testing PSRAM 32bpp 1024x576 HDMI1920x1080 VGA mode slow write",ccc,0)
  next i

    for i = 0 to 500
          ccc=getrnd() shl 8
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
      ccc=getrnd() shl 8
      v.fcircle(x1,y1,r,ccc)   
    next i  
    for i = 0 to 500
          ccc=getrnd() shl 8
      x1=getrnd() mod 1024
      x2=getrnd() mod 1024
      y1=getrnd() mod 576
      y2=getrnd() mod 576
      v.frame(x1,y1,x2,y2,ccc)
    next i  
  
    for i = 0 to 1000
      x1=getrnd() mod 1024
      y1=getrnd() mod 576
      r=getrnd() mod 100
         ccc=getrnd() shl 8
      v.circle(x1,y1,r,ccc) 
    next i  
  
    for i = 0 to 10000
         ccc=getrnd() shl 8
      x1=getrnd() mod 1024
      x2=getrnd() mod 200
      y1=getrnd() mod 576
      y2=getrnd() mod 200
      v.box(x1,y1,x1+x2,y1+y2,ccc)   
     next i    
    
loop


'/
let c1=1: let c2=0
'startmachine
startpsram
startvideo

dim ccc,x1,x2,y1,y2,r as ulong

do
  v.cls(154,147)
  v.setfontfamily(0)

  for j=1 to 2
    v.cls(200,0)
    print "Testing PSRAM 8bpp 1024x576 HDMI mode"
    waitms(5000)

    for i = 0 to 5000
      x1=getrnd() mod 1024
      y1=getrnd() mod 576
      r=getrnd() mod 100
      ccc=getrnd() and 255
      v.fcircle(x1,y1,r,ccc)   
    next i  

    for i = 0 to 500
      ccc=getrnd() and 255
      x1=getrnd() mod 1024
      x2=getrnd() mod 1024
      y1=getrnd() mod 576
      y2=getrnd() mod 576
      v.frame(x1,y1,x2,y2,ccc)
    next i  
  
    for i = 0 to 1000
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
  next j

loop
'/
