const xres=1024
const yres=576

class TWindow
          
  dim x,y as integer
  dim l,h,dh as ulong                     
  dim vcl,vch as ulong                   
  dim vcx,vcy as ulong
  dim canvas as ulong
  dim deco as ulong
  dim i,j as integer
  dim font_ptr,font_family as ulong
  dim blitbuf(1023) as ubyte
  dim cursor_x,cursor_y as ubyte
  dim write_color,write_background as ubyte
  dim ccc(1) as ulong
  dim winmailbox as ulong
  dim needclose,selected,visible,num,cog as ubyte
  
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

  if ((xx>=0) andalso (xx<l) andalso (yy>=0) andalso (yy<h)) then psram.fill(canvas+(l*yy+xx),c,1,0,1)
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
    psram.write(addr(ccccc),canvas+((y+yy) *l)+(x shl 2),8)
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

  psram.fill(canvas,bc,l*h,0,1)  
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
  if cursor_x>=2*l then
    cursor_x=0
    cursor_y+=1
    if cursor_y>=(h/16) then scrollup() : cursor_y=(h/16)-1
  endif 
360 end sub  
    
''---------- Output a char at the cursor position, move the cursor, don't react for tab or lf 

  sub putchar2(achar) 

  putcharxycgf(cursor_x,16*cursor_y,achar,write_color,write_background)
  cursor_x+=2 								   ' position granularity is char/2 which makes real centered text possible
  if cursor_x>=2*l then
    cursor_x=0
    cursor_y+=1
    if cursor_y>=(h/16) then scrollup() : cursor_y=(h/16)-1
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
  if (cursor_y>(h/16)-1) then scrollup : cursor_y=(h/16)-1
  end sub
  
''-----------  Scroll the window one text line up

  sub scrollup
	
  for i=0 to 16*(h/16)-17
    psram.read1(addr(blitbuf),canvas+(i+16)*l,l)
    psram.write(addr(blitbuf),canvas+i*l,l)
  next i
  for i=16*(h/16)-16 to 16*(h/16)-1: fastline(0,l-1,i,write_background) : next i
  end sub
 
''----------- Scroll the window one line down 

  sub scrolldown 

  for i=16*(h/16)-17 to 0 step -1
    psram.read1(addr(blitbuf), canvas+i*l, l)
    psram.write(addr(blitbuf), canvas+(i+16)*l, l) 
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
  outtextxycg(cursor_x,cursor_y,chr$(32),write_color,write_background)
  end sub
  
end class

class TRectangle
dim x1,x2,y1,y2,handle as short
end class

dim windows(7) as TWindow
dim points(31,1) as short
dim rectangles(141) as TRectangle  
dim rectnum as ubyte
dim xtable(31) as short
dim ytable(31) as short
dim vcount as ubyte

sub getrects



rectnum=0 			' Phase 1 - clear the rectangle list
var i=0

vcount=0                        ' Phase 2 - make a vertices list
for i=0 to 7
  if windows(i).visible then
    let x1=windows(i).x : let x2=windows(i).x+windows(i).l
    if deco=0 then let y1=windows(i).x else let y1=windows(i).x-22 // to do: make this configurable
    if deco=0 then let y1=windows(i).x else let y1=windows(i).x-22 // to do: make this configurable
    if x1<0 then x1=0
    if y1<0 then y1=0
    if x2>xres-1 then x2=xres-1
    if y2>yres-1 then y2=yres-1
    
    if vcount=0 then xtable(0)=x1 : ytable(0) = y1: vcount=1 : goto 500
        
    j=0
    do while (x1>xtable(j)) and (j<vcount) : j:=j+1 : loop 
    for k:=vcount to j+1 step -1 : xtable[k]:=xtable[k-1] : next k
    xtable[j]:=x1;
    j=0
    do while (y1>ytable(j)) and (j<vcount) : j:=j+1 : loop 
    for k:=vcount to j+1 step -1 : ytable[k]:=ytable[k-1] : next k
    ytable[j]:=y1;
    vcount+=1

500 j=0
    do while (x2>xtable(j)) and (j<vcount) : j:=j+1 : loop 
    for k:=vcount to j+1 step -1 : xtable[k]:=xtable[k-1] : next k
    xtable[j]:=x2;
    j=0
    do while (y2>ytable(j)) and (j<vcount) : j:=j+1 : loop 
    for k:=vcount to j+1 step -1 : ytable[k]:=ytable[k-1] : next k
    ytable[j]:=y2;
    vcount+=1
  endif
next i

' Phase 3 - delete duplicate vertices

i:=0;
do
  if (xtable(i)=xtable(i+1)) and (ytable(i)=ytable(i+1)) then
    for j=i+1 to vcount-1 
      xtable(j):=xtable(j+1);
      ytable(j):=ytable(j+1);
    next j  
    xtable(vcount-1)=0;
    ytable(vcount-1)=0;
    vcount-=1;
  endif
  i:=i+1;
loop until i>=vcount-1;

' Phase 4 - assign rectangles to windows - TODO 
  var j=0: do while windows(j).num=i: j+=1 : loop
  points(4*j,0)=windows(j).x: points(4*j,1)=windows(j).y
  points(4*j+1,0)=windows(j).x+windows(j).l-1: points(4*j+3,1)=windows(j).y
  points(4*j+2,0)=windows(j).x: points(4*j+3,1)=windows(j).y+windows(j).h+windows(j).dh-1
  points(4*j+3,0)=windows(j).x+windows(j).l-1: points(4*j+3,1)=windows(j).y+windows(j).h+windows(j).dh-1
next i

for i=0 to 31
  if points(i,0)<0 then points(i,0)=0
  if points(i,0)>1023 then points(i,0)=1023
  if points(i,1)<0 then points(i,1)=0
  if points(i,1)>1023 then points(i,1)=1023
next i
end sub  

/'
procedure getrectanglelist;

var rect,r2:PRectangle ;

    window:TWindow;
    dg,dh,dt,dl,dsh,dsv,dm:integer;
    x1,x2,y1,y2:integer;
    vcount,rcount:integer;

    ttttt: int64;

begin

' Phase 1 - clear the rectangle list

vcount:=0;
ttttt:=gettime;
// go to the top window and count vertices

while window<>nil do
  begin

  if window.decoration=nil then
    begin
    dg:=0;
    dh:=0;
    dt:=0;
    dl:=0;
    dsh:=0;
    dsv:=0;
    dm:=0;
    end
  else
    begin
    dt:=titleheight;
    dl:=borderwidth;
    dg:=borderwidth;
    dh:=borderwidth;
    if window.decoration.menu then dm:=menuheight else dm:=0;
    if window.decoration.hscroll then dsh:=scrollwidth else dsh:=0;
    if window.decoration.vscroll then dsv:=scrollwidth else dsv:=0;
    end ;

  x1:=window.x-dg; x2:=window.x+window.l+dg+dsv; if x2>=xres then x2:=xres; if x1<0 then x1:=0;
  y1:=window.y-dg-dm-dt; y2:=window.y+window.h+dg+dsh; if y2>=yres then y2:=yres; if y1<0 then y1:=0;
  if vcount=0 then begin xtable[vcount]:=x1; ytable[vcount]:=y1; inc(vcount); end
  else
    begin
    i:=0;
    while (x1>xtable[i]) and (i<vcount) do i:=i+1;
    for j:=vcount downto i+1 do xtable[j]:=xtable[j-1];
    xtable[i]:=x1;
    i:=0;
    while (y1>ytable[i]) and (i<vcount) do i:=i+1;
    for j:=vcount downto i+1 do ytable[j]:=ytable[j-1];
    ytable[i]:=y1;
    inc(vcount);
    end;


  i:=0;
  while (x2>xtable[i]) and (i<vcount) do i:=i+1;
  for j:=vcount downto i+1 do xtable[j]:=xtable[j-1];
  xtable[i]:=x2;
  i:=0;
  while (y2>ytable[i]) and (i<vcount) do i:=i+1;
  for j:=vcount downto i+1 do ytable[j]:=ytable[j-1];
  ytable[i]:=y2;
  inc(vcount);

  window:=window.next;        // go to the top window
  end;

// delete duplicate vertices

i:=0;
repeat
  if (xtable[i]=xtable[i+1]) and (ytable[i]=ytable[i+1]) then
    begin
    for j:=i+1 to vcount-1 do
      begin
      xtable[j]:=xtable[j+1];
      ytable[j]:=ytable[j+1];
      end;
    xtable[vcount-1]:=0;
    ytable[vcount-1]:=0;
    vcount-=1;
    end;
  i:=i+1;
until i>=vcount-1;



// make a rectangle list

rect:=@Arectangle;
for i:=0 to vcount-2 do
  for j:=0 to vcount-2 do
    begin
    if (xtable[j+1]>xtable[j]) and (ytable[i+1]>ytable[i]) then
      begin
      rect^.next:= new(PRectangle);
      rect^.next^.prev:=rect;
      rect^.next^.next:=nil;
      rect:=rect^.next;
      rect^.x1:=xtable[j];
      rect^.y1:=ytable[i];
      rect^.x2:=xtable[j+1]-1;
      rect^.y2:=ytable[i+1]-1;
      rect^.handle:=0;
      end;
    end;


// assign rectangles to windows

window:=background;

// go to the top window

while window.next<>nil do  window:=window.next;

// find rectangles in the window

while window<>nil do
  begin
    if window.decoration=nil then
    begin
    dg:=0;
    dh:=0;
    dt:=0;
    dl:=0;
    dsh:=0;
    dsv:=0;
    dm:=0;
    end
  else
    begin
    dt:=titleheight;
    dl:=borderwidth;
    dg:=borderwidth;
    dh:=borderwidth;
    if window.decoration.menu then dm:=menuheight else dm:=0;
    if window.decoration.hscroll then dsh:=scrollwidth else dsh:=0;
    if window.decoration.vscroll then dsv:=scrollwidth else dsv:=0;
    end ;

  rect:=Arectangle.next;
  x1:=window.x-dg; x2:=window.x+window.l+dg+dsv;
  y1:=window.y-dg-dm-dt; y2:=window.y+window.h+dg+dsh;



  while rect<>nil do begin
    if (rect^.x1>=x1) and (rect^.y1>=y1)and (rect^.x2<x2) and (rect^.y2<y2) and (rect^.handle=0) then rect^.handle:=integer(window);
    rect:=rect^.next;
    end;

  window:=window.prev;
  end ;

// merge adjacent rectangles

  rect:=Arectangle.next;
  while rect^.next<>nil do
    begin
    if (rect^.x2+1=rect^.next^.x1) and (rect^.handle=rect^.next^.handle) then
      begin
      r2:=rect^.next;
      rect^.x2:=rect^.next^.x2;
      rect^.next:=rect^.next^.next;
      dispose(r2);
      if rect^.next<>nil then rect^.next^.prev:=rect;
      end
    else
      rect:=rect^.next;
    end;


ttttt:=gettime-ttttt;

//-------debug

{
retromalina.box(500,0,100,50,0); retromalina.outtextxy(500,0,inttostr(ttttt),15);
retromalina.box(0,0,380,1000,0);
for i:=0 to vcount-1 do background.outtextxy8(280,8*i,inttostr(xtable[i])+' '+inttostr(ytable[i]),15);
rect:=Arectangle.next;
i:=0;
while rect<>nil do begin
                                              retromalina.outtextxy(0,i*16,inttostr(rect^.x1),15);
                                              retromalina.outtextxy(50,i*16,inttostr(rect^.y1),15);
                                              retromalina.outtextxy(100,i*16,inttostr(rect^.x2),15);
                                              retromalina.outtextxy(150,i*16,inttostr(rect^.y2),15);
                                              retromalina.outtextxy(200,i*16,inttostr(rect^.handle),15);
rect:=rect^.next;  i:=i+1;
end;}


end;  

'/

sub initwindows

for i=0 to 7: windows(i).num=255: next i
end sub

function createwindow(al,ah,ad) as ubyte

dim i as ubyte

i=0
do while i<8 andalso windows(i).num<>255
  i+=1
loop
if i>=8 then return 255

windows(i).h=ah
windows(i).l=al
windows(i).dh=22
windows(i).x=0
windows(i).y=0
windows(i).canvas=ad+22*al
windows(i).deco=ad
windows(i).vcl=0
windows(i).vch=0
windows(i).vcx=0
windows(i).vcy=0
windows(i).vcl=0
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
windows(i).num=i
windows(i).cog=0
windows(i).cls
return i
end function


/'

procedure getrectanglelist;

var rect,r2:PRectangle ;

    window:TWindow;
    dg,dh,dt,dl,dsh,dsv,dm:integer;
    x1,x2,y1,y2:integer;
    vcount,rcount:integer;

    ttttt: int64;

begin

if arectangle.handle=0 then       						'Arectangle is a root
  begin
  rect:=Arectangle.next;
  while rect^.next<>nil do rect:=rect^.next;
  while rect^.prev<>nil do begin rect:=rect^.prev; dispose(rect^.next); end;
  end;										'this clears the old rectangle list

window:=background;				' start from background
Arectangle.next:=nil;
Arectangle.prev:=nil;
Arectangle.x1:=0;
Arectangle.x2:=0;
Arectangle.y1:=0;
Arectangle.y2:=0;
Arectangle.handle:=0;    			' clear the first rectangle


vcount:=0;
ttttt:=gettime;
// go to the top window and count vertices

while window<>nil do
  begin

  if window.decoration=nil then
    begin
    dg:=0;
    dh:=0;
    dt:=0;
    dl:=0;
    dsh:=0;
    dsv:=0;
    dm:=0;
    end
  else
    begin
    dt:=titleheight;
    dl:=borderwidth;
    dg:=borderwidth;
    dh:=borderwidth;
    if window.decoration.menu then dm:=menuheight else dm:=0;
    if window.decoration.hscroll then dsh:=scrollwidth else dsh:=0;
    if window.decoration.vscroll then dsv:=scrollwidth else dsv:=0;
    end ;

  x1:=window.x-dg; x2:=window.x+window.l+dg+dsv; if x2>=xres then x2:=xres; if x1<0 then x1:=0;
  y1:=window.y-dg-dm-dt; y2:=window.y+window.h+dg+dsh; if y2>=yres then y2:=yres; if y1<0 then y1:=0;
  if vcount=0 then begin xtable[vcount]:=x1; ytable[vcount]:=y1; inc(vcount); end
  else
    begin
    i:=0;
    while (x1>xtable[i]) and (i<vcount) do i:=i+1;			' here are created x and y table
    for j:=vcount downto i+1 do xtable[j]:=xtable[j-1];
    xtable[i]:=x1;
    i:=0;
    while (y1>ytable[i]) and (i<vcount) do i:=i+1;
    for j:=vcount downto i+1 do ytable[j]:=ytable[j-1];
    ytable[i]:=y1;
    inc(vcount);
    end;


  i:=0;
  while (x2>xtable[i]) and (i<vcount) do i:=i+1;
  for j:=vcount downto i+1 do xtable[j]:=xtable[j-1];
  xtable[i]:=x2;
  i:=0;
  while (y2>ytable[i]) and (i<vcount) do i:=i+1;
  for j:=vcount downto i+1 do ytable[j]:=ytable[j-1];
  ytable[i]:=y2;
  inc(vcount);

  window:=window.next;        // go to the top window
  end;									' we have sorted tables of x's and y's 

// 									' delete duplicate vertices

i:=0;
repeat
  if (xtable[i]=xtable[i+1]) and (ytable[i]=ytable[i+1]) then
    begin
    for j:=i+1 to vcount-1 do
      begin
      xtable[j]:=xtable[j+1];
      ytable[j]:=ytable[j+1];
      end;
    xtable[vcount-1]:=0;
    ytable[vcount-1]:=0;
    vcount-=1;
    end;
  i:=i+1;
until i>=vcount-1;

'/// ---- here we have x and y lists

// make a rectangle list

rect:=@Arectangle;
for i:=0 to vcount-2 do
  for j:=0 to vcount-2 do
    begin
    if (xtable[j+1]>xtable[j]) and (ytable[i+1]>ytable[i]) then
      begin
      rect^.next:= new(PRectangle);
      rect^.next^.prev:=rect;
      rect^.next^.next:=nil;
      rect:=rect^.next;
      rect^.x1:=xtable[j];
      rect^.y1:=ytable[i];
      rect^.x2:=xtable[j+1]-1;
      rect^.y2:=ytable[i+1]-1;
      rect^.handle:=0;
      end;
    end;


// assign rectangles to windows

window:=background;

// go to the top window

while window.next<>nil do  window:=window.next;

// find rectangles in the window

while window<>nil do
  begin
    if window.decoration=nil then
    begin
    dg:=0;
    dh:=0;
    dt:=0;
    dl:=0;
    dsh:=0;
    dsv:=0;
    dm:=0;
    end
  else
    begin
    dt:=titleheight;
    dl:=borderwidth;
    dg:=borderwidth;
    dh:=borderwidth;
    if window.decoration.menu then dm:=menuheight else dm:=0;
    if window.decoration.hscroll then dsh:=scrollwidth else dsh:=0;
    if window.decoration.vscroll then dsv:=scrollwidth else dsv:=0;
    end ;

  rect:=Arectangle.next;
  x1:=window.x-dg; x2:=window.x+window.l+dg+dsv;
  y1:=window.y-dg-dm-dt; y2:=window.y+window.h+dg+dsh;



  while rect<>nil do begin
    if (rect^.x1>=x1) and (rect^.y1>=y1)and (rect^.x2<x2) and (rect^.y2<y2) and (rect^.handle=0) then rect^.handle:=integer(window);
    rect:=rect^.next;
    end;

  window:=window.prev;
  end ;

// merge adjacent rectangles

  rect:=Arectangle.next;
  while rect^.next<>nil do
    begin
    if (rect^.x2+1=rect^.next^.x1) and (rect^.handle=rect^.next^.handle) then
      begin
      r2:=rect^.next;
      rect^.x2:=rect^.next^.x2;
      rect^.next:=rect^.next^.next;
      dispose(r2);
      if rect^.next<>nil then rect^.next^.prev:=rect;
      end
    else
      rect:=rect^.next;
    end;


ttttt:=gettime-ttttt;

//-------debug

{
retromalina.box(500,0,100,50,0); retromalina.outtextxy(500,0,inttostr(ttttt),15);
retromalina.box(0,0,380,1000,0);
for i:=0 to vcount-1 do background.outtextxy8(280,8*i,inttostr(xtable[i])+' '+inttostr(ytable[i]),15);
rect:=Arectangle.next;
i:=0;
while rect<>nil do begin
                                              retromalina.outtextxy(0,i*16,inttostr(rect^.x1),15);
                                              retromalina.outtextxy(50,i*16,inttostr(rect^.y1),15);
                                              retromalina.outtextxy(100,i*16,inttostr(rect^.x2),15);
                                              retromalina.outtextxy(150,i*16,inttostr(rect^.y2),15);
                                              retromalina.outtextxy(200,i*16,inttostr(rect^.handle),15);
rect:=rect^.next;  i:=i+1;
end;}


end;
'/
