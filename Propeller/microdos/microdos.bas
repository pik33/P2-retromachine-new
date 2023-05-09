' MicroDOS - a binary loader for a PSRAM P2 system
' v. 0.04- 20230508
' Piotr Kardasz, pik33@o2.pl
' MIT license
' For better effect compile with -DFF_USE_LFN 
'-----------------------------------------------------------------------------------------------

const _clkfreq = 336956522 	' don't change, video drivers will set it to this

const pin=8			' VGA pin
const hpin=0			' HDMI pin
const up_key=$52		' define your keys here
const down_key=$51
const w_key=$1a
const s_key=$16
const enter_key=$28
const v_back_color=147		' Display colors for VGA and HDMI. Drivers use Atari type palette, 16 colors on high nibble, 16 luminances on low nibble, 32 is red, 112 is blue, 192 is green
const v_write_color=154
const h_back_color=147-16
const h_write_color=154-16
const v_vspace=40
const h_vspace=20


'-----------------------------------------------------------------------------------------------

#include "dir.bi"

dim v as class using "vg001.spin2"		' VGA driver
dim h as class using "hg008.spin2"		' HDMI driver
dim psram as class using "psram.spin2"		' PSRAM driver

'' All drivers are hacked for this. VGA and HDMI has to have separated buffers, PSRAM mailbuffers are moved to S7FF00 to make loading size as big as possible


'-----------------------------------------------------------------------------------------------

dim kbd as class using "usbnew.spin2"

'-----------------------------------------------------------------------------------------------

dim videocog as integer
dim hdmicog as integer
dim base as ulong
dim mbox as ulong

dim files$(27)
dim shortfiles$(27)
dim filebuf(16383) as ubyte
let title$="P2 MicroDOS v. 0.04 - 20230508"


function readkey() as ulong
return kbd.get_key()
end function

' Start cogs

let kbdcog=kbd.start()
psram.startx(0, 0, 12, 7)
mbox=psram.getMailbox(0)
videocog=v.start(pin,mbox)
v.cls(v_write_color,v_back_color)
hdmicog=h.start(hpin,mbox)
h.cls(h_write_color,h_back_color)

' Initialize the display

waitms(100)
v.setfontfamily(4)
v.outtextxycz(960-16*len(title$),v_vspace,title$,v_write_color,v_back_color,4,2)
h.setfontfamily(4)
h.outtextxycz(512-8*len(title$),h_vspace,title$,h_write_color,h_back_color,2,1)

mount "/sd", _vfs_open_sdcard()
chdir "/sd/microDOS"
let currentdir$="/sd/microDOS/"

let pos=3*v_vspace : let hpos=3*h_vspace
let filenum=0

' Find and list filenames (*.binary)

let filename$ = dir$("*", fbNormal)
while filename$ <> "" andalso filename$ <> nil
  let ext$=right$(filename$,7)
  if ext$=".binary" then
    files$(filenum)=currentdir$+filename$
    filename$=left$(filename$,len(filename$)-7)
    shortfiles$(filenum)=replacechar$(filename$,"_"," "): filenum+=1
  endif
  filename$ = dir$()
end while

let title$="W,S or up,down arrow to select, Enter to run"
v.outtextxycz(960-16*len(title$),1080-2*v_vspace,title$,v_write_color,v_back_color,4,2)
h.outtextxycz(512-8*len(title$),576-2*h_vspace,title$,h_write_color,h_back_color,2,1)

h.outtextxycz(512-4*len(shortfiles$(0)),hpos,shortfiles$(0),h_back_color,h_write_color,1,1) : hpos+=h_vspace
v.outtextxycz(960-8*len(shortfiles$(0)),pos,shortfiles$(0),v_back_color,v_write_color,2,2) : pos+=v_vspace
for i=1 to filenum-1: h.outtextxycz(512-4*len(shortfiles$(i)),hpos,shortfiles$(i),h_write_color,h_back_color,1,1) : hpos+=h_vspace : next i
for i=1 to filenum-1: v.outtextxycz(960-8*len(shortfiles$(i)),pos,shortfiles$(i),v_write_color,v_back_color,2,2) : pos+=v_vspace : next i

' Wait for a keyboard input, highlight the selected name

let filepos=0
do: loop until readkey()=0
do 
  let key=readkey() 
  if key=down_key orelse key=s_key then
    key=0
    v.outtextxycz(960-8*len(shortfiles$(filepos)),v_vspace*(3+filepos),shortfiles$(filepos),v_write_color,v_back_color,2,2) 
    h.outtextxycz(512-4*len(shortfiles$(filepos)), h_vspace*(3+filepos),shortfiles$(filepos),h_write_color,h_back_color,1,1) 
    filepos+=1
    if filepos>=filenum-1 then filepos=filenum-1
    v.outtextxycz(960-8*len(shortfiles$(filepos)),v_vspace*(3+filepos),shortfiles$(filepos),v_back_color,v_write_color,2,2) 
    h.outtextxycz(512-4*len(shortfiles$(filepos)), h_vspace*(3+filepos),shortfiles$(filepos),h_back_color,h_write_color,1,1) 
  endif  

  if key=up_key orelse key=w_key then
    key=0
    v.outtextxycz(960-8*len(shortfiles$(filepos)),v_vspace*(3+filepos),shortfiles$(filepos),v_write_color,v_back_color,2,2) 
    h.outtextxycz(512-4*len(shortfiles$(filepos)), h_vspace*(3+filepos),shortfiles$(filepos),h_write_color,h_back_color,1,1) 
    filepos-=1
    if filepos<0 then filepos=0
    v.outtextxycz(960-8*len(shortfiles$(filepos)),v_vspace*(3+filepos),shortfiles$(filepos),v_back_color,v_write_color,2,2) 
    h.outtextxycz(512-4*len(shortfiles$(filepos)), h_vspace*(3+filepos),shortfiles$(filepos),h_back_color,h_write_color,1,1) 
  endif  

' if enter, load the file

  if key=enter_key then
    let title$="Loading: " + files$(filepos)
    v.box(0,1080-64,1919,1080-33,v_back_color)
    v.outtextxycz(960-16*len(title$),1080-2*v_vspace,title$,v_write_color,v_back_color,4,2)
    h.box(0,576-32,1023,576-17,h_back_color)
    h.outtextxycz(512-8*len(title$),576-2*h_vspace,title$,h_write_color,h_back_color,2,1)
    key=0

' now upload it to PSRAM

    open files$(filepos) for input as #9
    let pos=1: let r=0 : let psramptr=0
    do
      get #9,pos,filebuf(0),16384,r : pos+=r	
      psram.write(addr(filebuf(0)),psramptr,16384)	
      psramptr+=r 					                                         ' move the buffer to the RAM and update RAM position. Todo: this can be done all at once
    loop until r<>16384  orelse psramptr>=$7C000  					         ' do until eof or memory full

' stop all driver cogs except PSRAM

    cpustop(kbdcog)
    cpustop(videocog)
    cpustop(hdmicog)

'start loading cog

    let loadingcog=cpu(@loadcog,@filebuf) 

' stop itself
    
    cpustop(cpuid())

  endif  
loop

'--------------------------- THE END OF THE MAIN PROGRAM ------------------------------------------------------

'----- addr: the helper function, return ulong instead of pointer

function addr(byref v as const any) as ulong
return(cast(ulong,@v))
end function


'----- the loader cog

		asm shared

             	org
loadcog      	cogid   t1              		' get a cogid
                mul     t1, #12                         ' compute the offset to PSRAM mailbox 
                add     mailbox, t1                     ' add offset to find this COG's mailbox

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
                cogid 	t1				' get id
                coginit #0,#0				' start the new program
                cogstop t1				' stop the loader

t1 		long 	0
mailbox 	long 	$7FF00
psramaddr 	long 	0

cmd             long    0
buf1            long    0
buf2            long    1024

		end asm
		
'----- The end ---------------------------------------------------------------------------------------------------
