class TWindow
          
  dim x,y as integer
  dim l,h as ulong                     
  dim vcl,vch as ulong                   
  dim vcx,vcy as ulong
  dim canvas as ulong
  dim i,j as integer
  dim font_ptr,font_family as ulong
  dim blitbuf(1023) as ubyte
  dim cursor_x,cursor_y as ubyte
  dim write_color,write_background as ubyte
  dim ccc(1) as ulong
  dim mailbox as ulong
  dim needclose,selected,visible, handle as ubyte
  
   
''---------- putpixel - put a pixel on the screen - a mother of all graphic functions ---------------------------

  sub putpixel(x,y,c)

  if ((x>=0) andalso (x<l) andalso (y>=0) andalso (y<h)) then psram.fill(canvas+(l*y+x),c,1,0,1)
  end sub
  
'----------- A line drawing family ------------------------------------------------------------------------------

  sub fastline(x1,x2,y,c)					         	' a fast 8bpp horizontal line

  if y<0 then return
  if x1>x2 then x1,x2=x2,x1
  psram.fill(canvas+(l*y+x1),c,1+x2-x1,0,1)
  end sub

  sub draw(x1,y1,x2,y2,c) 							' normal line
  
  dim d,dx,dy,ai,bi,xi,yi,x,y as integer					

  if (y1=y2) then
    fastline(x1,x2,y1,c)
  else  
    x=x1: y=y1
    if (x1<x2) then xi=1 : dx=x2-x1  else xi=-1 : dx:=x1-x2
    if (y1<y2) then yi=1 : dy=y2-y1  else yi=-1 : dy:=y1-y2
    putpixel(x,y,c)
    if (dx>dy) then 
      ai=(dy-dx)*2 : bi=dy*2 : d:= bi-dx
      do while (x<>x2) 
        if (d>=0) then x+=xi : y+=yi : d+=ai else d+=bi : x+=xi
        putpixel(x,y,c)
      loop
    else
      ai:=(dx-dy)*2 : bi:=dx*2 : d:=bi-dy
      do while (y<>y2)
        if (d>=0) then x+=xi : y+=yi : d+=ai else d+=bi : y+=yi
        putpixel(x, y,c)
    endif
  endif
  end sub
  

'-- A filled circle -----------------------------------------------------

  sub fcircle(x0,y0,r,c) 
  
  dim d,x,y,da,db

  d=5-4*r : x=0 : y=r :da=(-2*r+5)*4 :db:=3*4
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
  line(x1,y1,x1,y2,c)
  line(x2,y1,x2,y2,c)
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
    for xx from 0 to 7
      if (bb and (1 shl xx))<>0 then putpixel(xx+x,yy+y,f)
    next xx
  next yy
  end sub    
      
' ------  Opaque character      

  sub putcharxycg(x,y,achar,f,b) 
  
  dim bb as ubyte
  
  for yy=0 to 15
    bb=peek(font_ptr+(font_family shl 10)+ (achar shl 4) +yy)
    for xx from 0 to 7
      if (bb and (1 shl xx))<>0 then putpixel(xx+x,yy+y,f) else putpixel(xx+x,yy+y,b)
    next xx
  next yy
  end sub    
      
' ------  Opaque character fast, x unit=4 pixels         
      
  sub putcharxycgf(x,y,achar,f,b) 
  
  dim bb as ubyte
  dim c1,c2 as ulong

  for yy=0 to 15
    bb=peek(font_ptr+(font_family shl 10)+ (achar shl 4) +yy)

    	asm
  
  		testb bb,#0 wz
  	if_z 	setbyte c1,f,#0
  	if_nz setbyte c1,b,#0
 		testb bb,#1 wz
  	if_z 	setbyte c1,f,#1
  	if_nz setbyte c1,b,#1
  		testb bb,#2 wz
  	if_z 	setbyte c1,f,#2
  	if_nz setbyte c1,b,#2
  		testb bb,#3 wz
  	if_z 	setbyte c1,f,#3
  	if_nz setbyte c1,b,#3
  		testb bb,#4 wz
  	if_z 	setbyte c2,f,#0
  	if_nz setbyte c2,b,#0
  		testb bb,#5 wz
  	if_z 	setbyte c2,f,#1
  	if_nz setbyte c2,b,#1
  		testb bb,#6 wz
  	if_z 	setbyte c2,f,#2
  	if_nz setbyte c2,b,#2
  		testb bb,#7 wz
  	if_z 	setbyte c2,f,#3
  	if_nz setbyte c2,b,#3

 	end asm  
  
  ccc(0)=c1
  ccc(1)=c2 
  lpoke mailbox+8,8
  lpoke mailbox+4,addr(ccc(0))
  lpoke mailbox,canvas+((y+yy) shl 10)+(x shl 2)+$f0000000   
  do: loop until (lpeek(mailbox) and $F0000000)=0 
  end sub
 
      
' ------  Opaque  8x8 character      
 
pub putcharxycg8(x,y,achar,f,b) |xx, yy,bb

repeat yy from 0 to 7
  bb:=byte[@vga_font+font_family<<10+achar<<3+yy]
  repeat xx from 0 to 7
    if (bb&(1<<xx))<>0
      putpixel(xx+x,yy+y,f)
    else
      putpixel(xx+x,yy+y,b)      

'' ------  Opaque zoomed 8x16 character       

pub putcharxycz(x,y,achar,f,b,xz,yz) |xx,xxx,yy,yyy,bb

repeat yy from 0 to 15
  bb:=byte[@vga_font+font_family<<10+achar<<4+yy]
  repeat xx from 0 to 7
    if (bb&(1<<xx))<>0
      repeat yyy from 0 to yz-1
        repeat xxx from 0 to xz-1
          putpixel(xz*xx+xxx+x,yz*yy+yyy+y,f)
    else
      repeat yyy from 0 to yz-1
        repeat xxx from 0 to xz-1
          putpixel(xz*xx+xxx+x,yz*yy+yyy+y,b)
          
'' ----- String output using above          

pub outtextxycg(x,y,text,f,b) | iii,c

repeat iii from 0 to strsize(text)-1
  putcharxycg(x+8*iii,y,byte[text+iii],f,b)

pub outtextxycg8(x,y,text,f,b) | iii,c

repeat iii from 0 to strsize(text)-1
  putcharxycg8(x+8*iii,y,byte[text+iii],f,b)
  
pub outtextxycf(x,y,text,f) | iii,c

repeat iii from 0 to strsize(text)-1
  putcharxycf(x+8*iii,y,byte[text+iii],f)

pub outtextxycz(x,y,text,f,b,xz,yz) | iii,c

repeat iii from 0 to strsize(text)-1
  putcharxycz(x+8*xz*iii,y,byte[text+iii],f,b,xz,yz)


'**********************************************************************r***
'                                                                        *
' Font related functions                                                 *
'                                                                        *
'*************************************************************************

''--------- Set a font offset. TODO: remove, use byte#1 instead

pub setfontfamily(afontnum)

font_family:=afontnum
'if afontnum==8
'  font_ptr:=@amiga_font

if afontnum==4
  font_ptr:=@st_font
if afontnum==0
  font_ptr:=@vga_font
  

''--------- Get a pointer to a font definition

pub getfontaddr(num)

if num==1
  return @vga_font
if num==2
  return @st_font
if num==3
  return @a8_font  

''--------- Redefine a character



'*************************************************************************
'                                                                        *
'  Cursor functions                                                      *
'                                                                        *
'*************************************************************************


pub setcursorpos(x,y)

''---------- Set the (x,y) position of cursor

cursor_x:=x
cursor_y:=y


''---------- Wait for vblank. Amount=delay in frames

sub waitvbl(amount=1) 


for i from 1 to amount
  repeat until vblank==1
    waitus(100)
  repeat until vblank==0
    waitus(100)




''---------- Set colors for putchar, write and writeln

pub setwritecolors(ff,bb)

write_color:=ff
write_background:=bb

'*************************************************************************
'                                                                        *
'  Text functions                                                        *
'                                                                        *
'*************************************************************************

''---------- Clear the screen, set its foreground/background color  

pub cls(fc,bc)   :c,i

c:=bc
ram.fill(s_buf_ptr,c,4*buflen,0,1)  
setwritecolors(fc,bc)
cursor_x:=0
cursor_y:=0

''---------- Output a char at the cursor position, move the cursor 

pub putchar(achar) | c,x,y,l,newcpl

if achar==10
  crlf()
if achar==9
  cursor_x:=(cursor_x& %11110000)+16
  
if (achar<>9) && (achar<>10) 
  putcharxycgf(cursor_x,16*cursor_y,achar,write_color,write_background)
  cursor_x+=2

if cursor_x>=256
  cursor_x:=0
  cursor_y+=1
  if cursor_y>st_lines-1
    scrollup()
    cursor_y:=st_lines-1
    
''---------- Output a char at the cursor position, move the cursor, don't react for tab or lf 

  sub putchar2(achar) 

  putcharxycgf(cursor_x,16*cursor_y,achar,write_color,write_background)
  cursor_x+=2 								   ' position granularity is char/2 which makes real centered text possible
  if cursor_x>=2*l then
    cursor_x:=0
    cursor_y+=1
    if cursor_y>=(h/16) then scrollup() : cursor_y=(h/16)-1
  end sub  

''--------- Output a string at the cursor position, move the cursor  

  sub write(byref text$ as string) 

  for i=0 to len(text$)-1 : putchar2(peek(lpeek(addr(text$))+iii)) : next i
  end sub

''--------- Output a string at the cursor position x,y, move the cursor to the next line -

  sub writeln(byref text$ as string)

  write(text$)
  cursor_x=0
  cursor_y+=1
  if (cursor_y>st_lines-1) then scrollup : cursor_y=st_lines-1
  end sub
  
''-----------  Scroll the window one text line up

  sub scrollup
	
  for i=0 to 16*(h/16)-17
    ram.read1(addr(blitbuf),canvas+(i+16)*l,l)
    ram.write(addr(blitbuf),canvas+i*l,l)
  next i
  for i=560 to 575 : fastline(0,1023,i,write_background) : next i
  end sub
 
''----------- Scroll the window one line down 

  sub scrolldown 

  for i=16*(h/16)-17 to 0 step -1
    ram.read1(addr(blitbuf), canvas+i*l, l)
    ram.write(addr(blitbuf), canvas+(i+16)*l, l) 
  next i
  for i=0 to 15 : fastline(0,l,i,write_background) : next i   
  end sub

''----------- Set cursor at the first character in a new line, scroll if needed 

  sub crlf

  cursor_x=0
  cursor_y+=1
  if cursor_y>st_lines-1 then scrollup : cursor_y=st_lines-1
  end sub

''---------- Backspace. Move the cursor back, clear a character

  sub bksp

  cursor_x-=1 : if cursor_x=255 then cursor_x=(l/8)-1
  cursor_y-=1 : if cursor_y=255 then cursor_y=0 : scrollup
  outtextxycg(cursor_x,cursor_y," ",write_color,write_background)
  end sub
  
end class
