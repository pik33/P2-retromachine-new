class TWindow
          
  dim x,y as integer
  dim l,h as ulong                     
  dim vcl,vch as ulong                   
  dim vcx,vcy as ulong
  dim canvas as ulong
  dim needclose,selected,visible, handle as ubyte
  
  
 /' 
  
  ''---------- putpixel - put a pixel on the screen - a mother of all graphic functions ---------------------------


pub putpixel(x,y,c)

if ((x>=0) & (x<4*s_cpl) & (y>=0) & (y<s_lines))
  ram.fill(s_buf_ptr+(4*s_cpl1*y+x),c,1,0,1)

'----------- A line drawing family. BASIC doesn't like the name, so it is named "draw" and aliased  -----------------------------

pub fastline(x1,x2,y,c)									' a fast 8bpp horizontal line

if y<0
  return
if x1>x2 
  x1,x2:=x2,x1
ram.fill(s_buf_ptr+(4*s_cpl1*y+x1),c,1+x2-x1,0,1)

 
pub line(x1,y1,x2,y2,c)  								' this is a "draw" alias
draw(x1,y1,x2,y2,c)

pub draw(x1,y1,x2,y2,c) | d,dx,dy,ai,bi,xi,yi,x,y					' I had to rename the function for BASIC	

if (y1==y2)
  fastline(x1,x2,y1,c)
else  
  x:=x1
  y:=y1

  if (x1<x2) 
    xi:=1
    dx:=x2-x1
  else
    xi:=-1
    dx:=x1-x2
  
  if (y1<y2) 
    yi:=1
    dy:=y2-y1
  else
    yi:=-1
    dy:=y1-y2

  putpixel(x,y,c)

  if (dx>dy)
    ai:=(dy-dx)*2
    bi:=dy*2
    d:= bi-dx
    repeat while (x<>x2) 
      if (d>=0) 
        x+=xi
        y+=yi
        d+=ai
      else
        d+=bi
        x+=xi
      putpixel(x,y,c)
  else
    ai:=(dx-dy)*2
    bi:=dx*2
    d:=bi-dy
    repeat while (y<>y2)
      if (d>=0)
        x+=xi
        y+=yi
        d+=ai
      else
        d+=bi
        y+=yi
      putpixel(x, y,c)

'-- A filled circle -----------------------------------------------------

pub fcircle(x0,y0,r,c) | d,x,y,da,db

d:=5-4*r
x:=0
y:=r
da:=(-2*r+5)*4
db:=3*4
repeat while (x<=y) 
  fastline(x0-x,x0+x,y0-y,c)
  fastline(x0-x,x0+x,y0+y,c)
  fastline(x0-y,x0+y,y0-x,c)
  fastline(x0-y,x0+y,y0+x,c)
  if d>0 
    d+=da
    y-=1
    x+=1
    da+=4*4
    db+=2*4
  else
    d+=db
    x+=1
    da+=2*4
    db+=2*4
 
'-- A circle ------------------------------------------------------------ 
 
pub circle(x0,y0,r,c) | d,x,y,da,db

 
d:=5-4*r
x:=0
y:=r
da:=(-2*r+5)*4
db:=3*4
repeat while (x<=y) 
  putpixel(x0-x,y0-y,c)
  putpixel(x0-x,y0+y,c)
  putpixel(x0+x,y0-y,c)
  putpixel(x0+x,y0+y,c)
  putpixel(x0-y,y0-x,c)
  putpixel(x0-y,y0+x,c)
  putpixel(x0+y,y0-x,c)
  putpixel(x0+y,y0+x,c)
  if d>0 
    d+=da
    y-=1
    x+=1
    da+=4*4
    db+=2*4
  else
    d+=db
    x+=1
    da+=2*4
    db+=2*4
    
'-- A frame (an empty rectangle) ---------------------------------------

pub frame(x1,y1,x2,y2,c)

fastline(x1,x2,y1,c)
fastline(x1,x2,y2,c)
line(x1,y1,x1,y2,c)
line(x2,y1,x2,y2,c)

'-- A box (a filled rectangle) ----------------------------------------

pub box(x1,y1,x2,y2,c) |yy

repeat yy from y1 to y2
  fastline(x1,x2,yy,c)
    
'****************************************************************************************************************
'                                                                       		 			*
'  Characters on graphic screen                                          					*
'                                                                       					*
'****************************************************************************************************************    

' ------  Transparent character

pub putcharxycf(x,y,achar,f) |xx, yy, bb

repeat yy from 0 to 15
  bb:=byte[@vga_font+font_family<<10+achar<<4+yy]
  repeat xx from 0 to 7
    if (bb&(1<<xx))<>0
      putpixel(xx+x,yy+y,f)
      
' ------  Opaque character      

pub putcharxycg(x,y,achar,f,b) |xx, yy,bb

repeat yy from 0 to 15
  bb:=byte[@vga_font+font_family<<10+achar<<4+yy]
  repeat xx from 0 to 7
    if (bb&(1<<xx))<>0
      putpixel(xx+x,yy+y,f)
    else
      putpixel(xx+x,yy+y,b)
      
pub putcharxycgf(x,y,achar,f,b) |xx, yy,bb,c1,c2

 
repeat yy from 0 to 15

  bb:=byte[@vga_font+font_family<<10+achar<<4+yy]
  asm
  testb bb,#0 wz
  if_z setbyte c1,f,#0
  if_nz setbyte c1,b,#0
  testb bb,#1 wz
  if_z setbyte c1,f,#1
  if_nz setbyte c1,b,#1
  testb bb,#2 wz
  if_z setbyte c1,f,#2
  if_nz setbyte c1,b,#2
  testb bb,#3 wz
  if_z setbyte c1,f,#3
  if_nz setbyte c1,b,#3
  testb bb,#4 wz
  if_z setbyte c2,f,#0
  if_nz setbyte c2,b,#0
  testb bb,#5 wz
  if_z setbyte c2,f,#1
  if_nz setbyte c2,b,#1
  testb bb,#6 wz
  if_z setbyte c2,f,#2
  if_nz setbyte c2,b,#2
  testb bb,#7 wz
  if_z setbyte c2,f,#3
  if_nz setbyte c2,b,#3

   
  endasm  
  
  ccc[0]:=c1
  ccc[1]:=c2 
  long[mailbox0][2]:=8
  long[mailbox0][1]:=@ccc
  long[mailbox0]:= s_buf_ptr+((y+yy)<<10+x<<2)+$f0000000   
  repeat
  while long[mailbox0] < 0 
 
 
      
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

pub defchar(fn,ch,b0,b1,b2,b3,b4,b5,b6,b7,b8,b9,b10,b11,b12,b13,b14,b15) :s

s:=@st_font+fn+ch*16
byte[s+00]:=b0
byte[s+01]:=b1
byte[s+02]:=b2
byte[s+03]:=b3
byte[s+04]:=b4
byte[s+05]:=b5
byte[s+06]:=b6
byte[s+07]:=b7
byte[s+08]:=b8
byte[s+09]:=b9
byte[s+10]:=b10
byte[s+11]:=b11
byte[s+12]:=b12
byte[s+13]:=b13
byte[s+14]:=b14
byte[s+15]:=b15

'*************************************************************************
'                                                                        *
'  Cursor functions                                                      *
'                                                                        *
'*************************************************************************


pub setcursorpos(x,y)

''---------- Set the (x,y) position of cursor

cursor_x:=x
cursor_y:=y


'*************************************************************************
'                                                                        *
'  VBlank functions                                                      *
'                                                                        *
'*************************************************************************

pub waitvbl(amount) | i

''---------- Wait for start of vblank. Amount=delay in frames

repeat i from 1 to amount
  repeat until vblank==0
    waitus(100)
  repeat until vblank==1
    waitus(100)


pub waitvblend(amount) | i

''---------- Wait for end of vblank. Amount=delay in frames

repeat i from 1 to amount
  repeat until vblank==1
    waitus(100)
  repeat until vblank==0
    waitus(100)

'*************************************************************************
'                                                                        *
'  Color functions                                                       *
'                                                                        *
'*************************************************************************

''---------- Get a VGA color code

pub getvgacolor(color):r

return colors[color]

pub getpalettecolor(color):r

return long[palette_ptr+4*color]



''---------- Set the border color

pub setbordercolor(r,g,b) | color

color:=r<<24+g<<16+b<<8
bordercolor:=color

pub setbordercolor2(color) 

bordercolor:=color


''---------- Set colors for putchar, write and writeln

pub setwritecolors(ff,bb)

write_color:=ff
write_background:=bb

''---------- Set color #c in palette to r,g,b

pub setcolor(c,r,g,b)  |cc

cc:=r<<24+g<<16+b<<8
long[palette_ptr+4*c]:=cc


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

pub putchar2(achar) | c,x,y,l,newcpl

putcharxycgf(cursor_x,16*cursor_y,achar,write_color,write_background)
cursor_x+=2
if cursor_x>=256
  cursor_x:=0
  cursor_y+=1
  if cursor_y>st_lines-1
    scrollup()
    cursor_y:=st_lines-1

''--------- Output a string at the cursor position, move the cursor  

pub write(text) | iii,c,ncx,ncy

repeat iii from 0 to strsize(text)-1
  putchar2(byte[text+iii])

'--------- Output a string at the cursor position x,y, move the cursor to the next line -

pub writeln(text)

write(text)
cursor_x:=0
cursor_y+=1
if (cursor_y>st_lines-1)
  scrollup()
  cursor_y:=st_lines-1

''-----------  Scroll the screen one line up

pub scrollup() | i
	
repeat i from 0 to 559 
  ram.read1($80000-4096-1024-s_debug, s_buf_ptr+(i+16)*4*s_cpl1, 4*s_cpl1)
  ram.write($80000-4096-1024-s_debug, s_buf_ptr+i*4*s_cpl1, 4*s_cpl1)

repeat i from 560 to 575
   fastline(0,1023,i,write_background)   
 
''----------- Scroll the screen one line down 

pub scrolldown() | i

repeat i from 559 to 0
  ram.read1($80000-4096-1024-s_debug, s_buf_ptr+i*4*s_cpl1, 4*s_cpl1)
  ram.write($80000-4096-1024-s_debug, s_buf_ptr+(i+16)*4*s_cpl1, 4*s_cpl1)

repeat i from 0 to 15
   fastline(0,1023,i,write_background)      

''----------- Set cursor at the first character in a new line, scroll if needed 

pub crlf()

cursor_x:=0
cursor_y+=1
if cursor_y>st_lines-1
  scrollup()
  cursor_y:=st_lines-1

''---------- Backspace. Move the cursor back, clear a character

pub bksp()

cursor_x-=1
if cursor_x==255
  cursor_x:=s_cpl-1
  cursor_y-=1
  if cursor_y==255
    cursor_y:=0
    scrollup()

outtextxycg(cursor_x,cursor_y,string(" "),write_color,write_background)


  
  
'/  
  
  
end class
