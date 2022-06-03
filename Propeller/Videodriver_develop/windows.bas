const xres=1024
const yres=576

class TWindow
          
  dim x,y as integer			' position on screen
  dim l,h,dh as ulong                   ' length, height, deco height  
  dim wl,wh as ulong                  ' on screen length and height 
  dim vx,vy as ulong                  ' x,y offset of the window
  dim canvas as ulong                   ' canvas pointer
  dim deco as ulong                     ' deco pointer
  dim i,j as integer			' helper variables local in class
  dim font_ptr,font_family as ulong     
  dim blitbuf(1023) as ubyte            ' local hub blit buffer
  dim cursor_x,cursor_y as ubyte
  dim write_color,write_background as ubyte
  dim ccc(1) as ulong
  dim winmailbox as ulong
  dim needclose,selected,visible,num,cog as ubyte
  dim title$ as string
  dim tl as integer
  
  dim psram as class using "/home/pik33/Programowanie/P2-retromachine/Propeller/Videodriver_develop/psram4.spin2" ' full path needed here
  

  sub psread(hub,ram,cnt)
  
  dim r as ulong
  
  lpoke winmailbox+8,cnt
  lpoke winmailbox+4,hub
  lpoke winmailbox,$A0000000 + ram
  do
    let r = lpeek(winmailbox)    
  loop while r and $80000000 <> 0   
  end sub    
  
    sub pswrite(hub,ram,cnt)
    dim r as ulong
  lpoke winmailbox+8,cnt
  lpoke winmailbox+4,hub
  lpoke winmailbox,$F0000000 + ram
  do
    let r = lpeek(winmailbox)    
  loop while r and $80000000 <> 0    
  end sub  
  
''---------- putpixel - put a pixel on the screen - a mother of all graphic functions ---------------------------

  sub putpixel(xx as ulong, yy as ulong, c as ubyte)

  if ((xx>=0) andalso (xx<wl) andalso (yy>=0) andalso (yy<wh)) then psram.fill(canvas+(wl*yy+xx),c,1,0,1)
  end sub
  
'----------- A line drawing family ------------------------------------------------------------------------------

  sub fastline(x1,x2,y,c)					         	' a fast 8bpp horizontal line

  if y<0 then return
  if x1>x2 then x1,x2=x2,x1
  psram.fill(canvas+(wl*y+x1),c,1+x2-x1,0,1)
  end sub

  sub draw(x1,y1,x2,y2,c) 							' normal line
  
  dim d,dx,dy,ai,bi,xi,yi,x,y as integer					

  if (y1=y2) then
    fastline(x1,x2,y1,c)
  else  
    x=x1: y=y1
    if (x1<x2) then xi=1 : dx=x2-x1  else xi=-1 : dx=x1-x2
    if (y1<y2) then yi=1 : dy=y2-y1  else yi=-1 : dy=y1-y2
    putpixel(x,y,c)
    if (dx>dy) then 
      ai=(dy-dx)*2 : bi=dy*2 : d= bi-dx
      do while (x<>x2) 
        if (d>=0) then x+=xi : y+=yi : d+=ai else d+=bi : x+=xi
        putpixel(x,y,c)
      loop
    else
      ai=(dx-dy)*2 : bi=dx*2 : d=bi-dy
      do while (y<>y2)
        if (d>=0) then x+=xi : y+=yi : d+=ai else d+=bi : y+=yi
        putpixel(x,y,c)
      loop  
    endif
  endif
  end sub
  

'-- A filled circle -----------------------------------------------------

  sub fcircle(x0,y0,r,c) 
  
  dim d,x,y,da,db

  d=5-4*r : x=0 : y=r :da=(-2*r+5)*4 :db=3*4
  do while (x<=y) 
    fastline(x0-x,x0+x,y0-y,c)
    fastline(x0-x,x0+x,y0+y,c)
    fastline(x0-y,x0+y,y0-x,c)
    fastline(x0-y,x0+y,y0+x,c)
    if d>0 then d+=da : y-=1 : x+=1 : da+=4*4 : db+=2*4 else d+=db : x+=1 : da+=2*4 : db+=2*4
  loop
  end sub 
 

'-- A circle ------------------------------------------------------------ 
 
  sub circle(x0,y0,r,c) 
  
  dim d,x,y,da,db

  d=5-4*r : x=0 : y=r :da=(-2*r+5)*4 : db=3*4
  do while (x<=y) 
    putpixel(x0-x,y0-y,c)
    putpixel(x0-x,y0+y,c)
    putpixel(x0+x,y0-y,c)
    putpixel(x0+x,y0+y,c)
    putpixel(x0-y,y0-x,c)
    putpixel(x0-y,y0+x,c)
    putpixel(x0+y,y0-x,c)
    putpixel(x0+y,y0+x,c)
  if d>0 then d+=da : y-=1 :  x+=1 : da+=4*4 : db+=2*4 else d+=db : x+=1 : da+=2*4 : db+=2*4
  loop 
  end sub
    
        
'-- A frame (an empty rectangle) ---------------------------------------

  sub frame(x1,y1,x2,y2,c)

  fastline(x1,x2,y1,c)
  fastline(x1,x2,y2,c)
  draw(x1,y1,x1,y2,c)
  draw(x2,y1,x2,y2,c)
  end sub

'-- A box (a filled rectangle) ----------------------------------------

  sub box(x1,y1,x2,y2,c) 

  for yy=y1 to y2
    fastline(x1,x2,yy,c)
  next yy
  end sub
 
    
'****************************************************************************************************************
'                                                                       		 			*
'  Characters on graphic screen                                          					*
'                                                                       					*
'****************************************************************************************************************    

' ------  Transparent character

  sub putcharxycf(x,y,achar,f)  
  
  dim bb as ubyte

  for yy=0 to 15
    bb=peek(font_ptr+(font_family shl 10) + (achar shl 4) + yy)
    for xx=0 to 7
      if (bb and (1 shl xx))<>0 then putpixel(xx+x,yy+y,f)
    next xx
  next yy
  end sub    
      
' ------  Opaque character      

  sub putcharxycg(x,y,achar,f,b) 
  
  dim bb as ubyte
  
  for yy=0 to 15
    bb=peek(font_ptr+(font_family shl 10)+ (achar shl 4) +yy)
    for xx=0 to 7
      if (bb and (1 shl xx))<>0 then putpixel(xx+x,yy+y,f) else putpixel(xx+x,yy+y,b)
    next xx
  next yy
  end sub    
      
' ------  Opaque character fast, x unit=4 pixels         
  dim ccccc(1) as ulong   
      
  sub putcharxycgf(x,y,achar,f,b) 
  
  dim bbb as ubyte
  dim cc1,cc2 as ulong

  for yy=0 to 15
    bbb=peek(font_ptr+(font_family shl 10)+ (achar shl 4) +yy)

    	const asm
  
  		testb bbb,#0 wz
  	if_z 	setbyte cc1,f,#0
  	if_nz setbyte cc1,b,#0
 		testb bbb,#1 wz
  	if_z 	setbyte cc1,f,#1
  	if_nz setbyte cc1,b,#1
  		testb bbb,#2 wz
  	if_z 	setbyte cc1,f,#2
  	if_nz setbyte cc1,b,#2
  		testb bbb,#3 wz
  	if_z 	setbyte cc1,f,#3
  	if_nz setbyte cc1,b,#3
  		testb bbb,#4 wz
  	if_z 	setbyte cc2,f,#0
  	if_nz setbyte cc2,b,#0
  		testb bbb,#5 wz
  	if_z 	setbyte cc2,f,#1
  	if_nz setbyte cc2,b,#1
  		testb bbb,#6 wz
  	if_z 	setbyte cc2,f,#2
  	if_nz setbyte cc2,b,#2
  		testb bbb,#7 wz
  	if_z 	setbyte cc2,f,#3
  	if_nz setbyte cc2,b,#3

 	end asm  
  
    ccccc(0)=cc1
    ccccc(1)=cc2 
    psram.write(addr(ccccc),canvas+((y+yy) *wl)+(x shl 2),8)
'    lpoke winmailbox+8,8
'    lpoke winmailbox+4,addr(ccccc)
 '   lpoke winmailbox,canvas+((y+yy) *l)+(x shl 2)+$f0000000   
 '   do: loop until (lpeek(winmailbox) and $80000000)=0 
    next yy
  end sub
 
      
' ------  Opaque  8x8 character      
 
  sub putcharxycg8(x,y,achar,f,b)  

  dim bb as ubyte
  
  for yy=0 to 7
    bb=peek(font_ptr+(font_family shl 10)+ (achar shl 4) +yy)
    for xx=0 to 7
      if (bb and (1 shl xx))<>0 then putpixel(xx+x,yy+y,f) else putpixel(xx+x,yy+y,b)      
    next xx
  next yy
  end sub  

'' ------  Opaque zoomed 8x16 character       

  sub putcharxycz(x,y,achar,f,b,xz,yz)  
  
  dim bb as ubyte

  for yy=0 to 15
    bb=peek(font_ptr+(font_family shl 10)+ (achar shl 4) +yy)
    for xx=0 to 7
      if (bb and (1 shl xx))<>0 then 
        for yyy=0 to yz-1
          for xxx=0 to xz-1
            putpixel(xz*xx+xxx+x,yz*yy+yyy+y,f)
          next xxx
        next yyy      
      else
        for yyy=0 to yz-1
          for xxx=0 to xz-1
            putpixel(xz*xx+xxx+x,yz*yy+yyy+y,b)
          next xxx
        next yyy 
      endif
    next xx
  next yy
  end sub      
          
'' ----- String output using above          

  sub outtextxycg(x,y,text as const ubyte pointer,f,b)  

  var i=0 : do while peek(text+i)<>0 : putcharxycg(x+8*i,y,peek(text+i),f,b) : i+=1: loop
  end sub

  sub outtextxycg8(x,y,text$,f,b)  

  for i=0 to len(text$)-1 : putcharxycg8(x+8*i,y,peek(lpeek(addr(text$))+i),f,b) : next i
  end sub
  
  sub outtextxycf(x,y,text$,f) 

  for i=0 to len(text$)-1 : putcharxycf(x+8*i,y,peek(lpeek(addr(text$))+i),f) : next i
  end sub

  sub outtextxycz(x,y,text,f,b,xz,yz) 

  for i=0 to len(text$)-1 : putcharxycz(x+8*xz*iii,y,peek(text+i),f,b,xz,yz) :next i
  end sub


'*************************************************************************
'                                                                        *
'  Cursor functions                                                      *
'                                                                        *
'*************************************************************************

 sub setcursorpos(x,y)

''---------- Set the (x,y) position of cursor

  cursor_x=x
  cursor_y=y
  end sub

''---------- Wait for vblank. Amount=delay in frames

  sub waitvbl(amount=1) 

  for i=1 to amount
    do : waitus (10) : loop until v.vblank=1
    do : waitus (10) : loop until v.vblank=0
  next i
  end sub

''---------- Set colors for putchar, write and writeln

  sub setwritecolors(ff,bb)

  write_color=ff
  write_background=bb
  end sub

'*************************************************************************
'                                                                        *
'  Text functions                                                        *
'                                                                        *
'*************************************************************************

''---------- Clear the screen, set its foreground/background color  

  sub cls(fc=154,bc=147)  

  psram.fill(canvas,bc,wl*wh,0,1)  
  setwritecolors(fc,bc)
  cursor_x=0
  cursor_y=0
  end sub

''---------- Output a char at the cursor position, move the cursor 

  sub putchar(achar) 

  if achar=10 then crlf : goto 360
  if achar=9  then cursor_x=(cursor_x and  %11110000)+16 : goto 360
    
  putcharxycgf(cursor_x,16*cursor_y,achar,write_color,write_background)
  cursor_x+=2
  if cursor_x>=wl/4 then
    cursor_x=0
    cursor_y+=1
    if cursor_y>=(wh/16) then scrollup() : cursor_y=(wh/16)-1
  endif 
360 end sub  
    
''---------- Output a char at the cursor position, move the cursor, don't react for tab or lf 

  sub putchar2(achar) 

  putcharxycgf(cursor_x,16*cursor_y,achar,write_color,write_background)
  cursor_x+=2 								   ' position granularity is char/2 which makes real centered text possible
  if cursor_x>=wl/4 then
    cursor_x=0
    cursor_y+=1
    if cursor_y>=(wh/16) then scrollup() : cursor_y=(wh/16)-1
  endif  
  end sub  

''--------- Output a string at the cursor position, move the cursor  

  sub write(text as const ubyte pointer) 


  var i=0 : do while peek(text+i)<>0 : putchar2(peek(text+i)) : i+=1 : loop
  end sub

''--------- Output a string at the cursor position x,y, move the cursor to the next line -

  sub writeln(text$ as string)

  write(text$)
  cursor_x=0
  cursor_y+=1
  if (cursor_y>(wh/16)-1) then scrollup : cursor_y=(wh/16)-1
  end sub
  
''-----------  Scroll the window one text line up

  sub scrollup
	
  for i=0 to 16*(wh/16)-17
    psram.read1(addr(blitbuf),canvas+(i+16)*wl,wl)
    psram.write(addr(blitbuf),canvas+i*wl,wl)
  next i
  for i=16*(wh/16)-16 to 16*(wh/16)-1: fastline(0,wl-1,i,write_background) : next i
  end sub
 
''----------- Scroll the window one line down 

  sub scrolldown 

  for i=16*(wh/16)-17 to 0 step -1
    psram.read1(addr(blitbuf), canvas+i*wl, wl)
    psram.write(addr(blitbuf), canvas+(i+16)*wl, lw) 
  next i
  for i=0 to 15 : fastline(0,wl,i,write_background) : next i   
  end sub

''----------- Set cursor at the first character in a new line, scroll if needed 

  sub crlf

  cursor_x=0
  cursor_y+=1
  if cursor_y>st_lines-1 then scrollup : cursor_y=st_lines-1
  end sub

''---------- Backspace. Move the cursor back, clear a character

  sub bksp

  cursor_x-=1 : if cursor_x=255 then cursor_x=(wl/4)-1
  cursor_y-=1 : if cursor_y=255 then cursor_y=0 : scrollup
  outtextxycg(cursor_x,cursor_y,chr$(32),write_color,write_background)
  end sub
  
  sub show
  visible=1
  end sub
  
  sub hide
  visible=0
  end sub
  

  sub move(ax,ay,al,ah,avx,avy)

  if ay>yres-22 then ay=yres-22
  if ax>xres-22 then ax=xres-22

  if al>wl then al=wl
  if al>0 then l=al 
  if ah>wh then ah=wh 
  if (ah>0) andalso (ah<64) then ah=64
  if ah>0 then h=ah

var q=8*tl+96
if (deco>0) andalso (al>0) andalso (al<q) then l=q

if ax>-2048 then x=ax
if ay>-2048 then y=ay
if avx>-1 then vx=avx
if avy>-1 then vy=avy

end sub

end class

'''-------------------------------------------------------------- End of TWindow class ---------------------------------------------------------------------

class TRectangle
 dim x1,x2,y1,y2,handle,dummy as short
end class

''' ------------------------------------------------------------

dim windows(7) as TWindow
dim order(7) as ubyte
dim points(31,1) as short
dim rectangles(255) as TRectangle  
dim rectnum as ubyte
dim xtable(31) as short
dim ytable(31) as short
dim vcount, wincount as ubyte

'' ---------------------------------------------------------- Getrect - split the screen to rectangles by windows --------------------------------------------------

sub getrects

dim x1,x2,y1,y2 as short

rectnum=0 			' Phase 1 - clear the rectangle list
dim i,j as integer
let ttm=getct()
vcount=0                        ' Phase 2 - make a vertices list
for i=0 to 7
  if windows(i).visible then
    x1=windows(i).x : x2=windows(i).x+windows(i).l
    if windows(i).deco=0 then y1=windows(i).y else y1=windows(i).y-22 '// to do: make this configurable
    if windows(i).deco=0 then y2=windows(i).y+windows(i).h else y1=windows(i).y+windows(i).wh+windows(i).h-22 '// to do: make this configurable
    if x1<0 then x1=0
    if y1<0 then y1=0
    if x2>xres then x2=xres
    if y2>yres then y2=yres
    if vcount=0 then xtable(0)=x1 : ytable(0) = y1: vcount=1 : goto 500
        
    j=0
    do while (x1>xtable(j)) and (j<vcount) : j=j+1 : loop 
    for k=vcount to j+1 step -1 : xtable[k]=xtable[k-1] : next k
    xtable(j)=x1
    j=0
    do while (y1>ytable(j)) and (j<vcount) : j=j+1 : loop 
    for k=vcount to j+1 step -1 : ytable[k]=ytable[k-1] : next k
    ytable(j)=y1
    vcount+=1

500 j=0
    do while (x2>xtable(j)) and (j<vcount) : j=j+1 : loop 
    for k=vcount to j+1 step -1 : xtable[k]=xtable[k-1] : next k
    xtable(j)=x2
    j=0
    do while (y2>ytable(j)) and (j<vcount) : j=j+1 : loop 
    for k=vcount to j+1 step -1 : ytable[k]=ytable[k-1] : next k
    ytable(j)=y2
    vcount+=1
  endif
next i

' Phase 3 - delete duplicate vertices

i=0
do
  if (xtable(i)=xtable(i+1)) and (ytable(i)=ytable(i+1)) then
    longmove(@xtable(i+1),@xtable(i+2),vcount-i-2)
    longmove(@ytable(i+1),@ytable(i+2),vcount-i-2)
    
 '   for j=i+1 to vcount-1 
 '     xtable(j)=xtable(j+1)
 '     ytable(j)=ytable(j+1)
 '   next j  
 '   xtable(vcount-1)=0
 '   ytable(vcount-1)=0
    vcount-=1
  endif
  i=i+1
loop until i>=vcount-1


 
''''print vcount
'''for i=0 to vcount-1: print xtable(i),ytable(i): next i 

' Phase 4: make a rectangle list

let rectnum=0
for i=0 to vcount-2
  for j=0 to vcount-2 
    if (xtable(j+1)>xtable(j)) and (ytable(i+1)>ytable(i)) then
      rectangles(rectnum).x1=xtable[j]
      rectangles(rectnum).y1=ytable[i]
      rectangles(rectnum).x2=xtable[j+1]-1
      rectangles(rectnum).y2=ytable[i+1]-1
      rectangles(rectnum).handle=-1
      rectnum+=1
    endif
  next j
next i  



' Phase 5 - assign rectangles to windows ''''' todo win order has to be kept somewhere to make find easy

var maxnum=-1 : var maxidx=-1
for i=0 to 7
  if windows (i).visible<>0  then maxnum=i
next i  

''''''print maxnum
' find rectangles in the window
for i=maxnum to 0 step -1
  if windows(i).visible<>0 then
    x1=windows(i).x : x2=windows(i).x+windows(i).l
    if windows(i).deco=0 then y1=windows(i).y else y1=windows(i).y-22 'to do: make this configurable
    if windows(i).deco=0 then y2=windows(i).y+windows(i).h else y1=windows(i).y+windows(i).h+windows(i).h-22 'to do: make this configurable
    if x1<0 then x1=0
    if y1<0 then y1=0
    if x2>xres then x2=xres
    if y2>yres then y2=yres
    for j=0 to rectnum -1
      if rectangles(j).x1>=x1 andalso rectangles(j).y1>=y1 andalso rectangles(j).x2<x2 andalso rectangles(j).y2<y2 andalso rectangles(j).handle=-1 then rectangles(j).handle=i 
    next j
  endif
next i    
'''''print rectnum
''''for i=0 to rectnum-1: print rectangles(i).x1,rectangles(i).y1,rectangles(i).x2,rectangles(i).y2, rectangles(i).handle : next i: print rectnum 

' Phase 6 - merge adjacent rectangles

i=0 : let kwas=0
do
  if (rectangles(i).x2+1=rectangles(i+1).x1) andalso (rectangles(i).handle=rectangles(i+1).handle) then
    rectangles(i).x2=rectangles(i+1).x2
    longmove(@rectangles(i+1).x1,@rectangles(i+2).x1,3*(rectnum-i-2))
    rectnum-=1
    i-=1
  endif
  i=i+1
loop until i>=rectnum-1
let ttm=getct()-ttm': print ttm/336
'''print
'''print rectnum
'''for i=0 to rectnum-1: print rectangles(i).x1,rectangles(i).y1,rectangles(i).x2,rectangles(i).y2, rectangles(i).handle : next i
end sub  

'' ---------------------------------------------------------------------------------------------------------
dim list(575,60)

sub dolist
longfill(@list(0,0),0,16*576)
let ttm=getct()
for i=0 to rectnum-1
  for j=rectangles(i).y1 to rectangles(i).y2
    list(j,list(j,60))=rectangles(i).handle+1 : list(j,60)+=1
  next j  
next i
let ttm=getct()-ttm

end sub



''--------------------------------------------------------------------------------------------------------------
sub initwindows
for i=0 to 7: windows(i).num=255 : order(i)=i : next i
wincount=0
end sub

function createwindow(al,ah,ad,canvas) as ubyte

dim i as ubyte

if wincount>=8 then return 255

i=0
do while i<8 andalso windows(i).num<>255 '' find first free slot
  i+=1
loop
if i>=8 then return 255

order(wincount)=i  ' this will be a top window

windows(i).wh=ah
windows(i).wl=al
windows(i).dh=22
windows(i).x=0
windows(i).y=0
windows(i).canvas=canvas 'ad+22*al
windows(i).deco=ad
windows(i).l=0
windows(i).h=0
windows(i).vx=0
windows(i).vy=0
windows(i).font_ptr=v.font_ptr
windows(i).font_family=0
windows(i).cursor_x=0
windows(i).cursor_y=0
windows(i).write_color=154
windows(i).write_background=147
windows(i).winmailbox=mbox+12*i
windows(i).needclose=0
windows(i).selected=0
windows(i).visible=0
windows(i).num=wincount
windows(i).cog=0
windows(i).cls
return i
end function


sub move(handle,ax,ay,al,ah,avx,avy)
windows(handle).move(ax,ay,al,ah,avx,avy)
end sub
