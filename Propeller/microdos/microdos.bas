const _clkfreq = 336956522


#include "dir.bi"

dim v as class using "vg001.spin2"
dim rm as class using "retrocog.spin2"
dim psram as class using "psram.spin2"

dim videocog as integer
dim base as ulong
dim mbox as ulong

'#include "retromachine.bi"
 
let pin=8
dim files$(27)
dim shortfiles$(27)
dim filebuf(16383) as ubyte
let title$="P2 MicroDOS v. 0.01 - 20220613"

let retrocog=rm.start()
psram.startx(0, 0, 12, 7)
mbox=psram.getMailbox(0)
videocog=v.start(pin,mbox)
v.cls(154,147)


for thecog=0 to 7:psram.setQos(thecog, 112 << 16) :next thecog
psram.setQoS(videocog, $7FFFf400)

open SendRecvDevice(@v.putchar, nil, nil) as #0

waitms(100)
v.setfontfamily(4)
v.outtextxycz(960-16*len(title$),32,title$,154,147,4,2)

mount "/sd", _vfs_open_sdcard()
chdir "/sd/microDOS"
let currentdir$="/sd/microDOS/"

let pos=96
let filenum=0

let filename$ = dir$("*", fbNormal)
while filename$ <> "" andalso filename$ <> nil
  let ext$=right$(filename$,7)
  if ext$=".binary" then
    files$(filenum)=currentdir$+filename$
    filename$=left$(filename$,len(filename$)-7)
    shortfiles$(filenum)=filename$: filenum+=1
  endif
  filename$ = dir$()
end while

let title$="W,S or up,down arrow to select, Enter to run"
v.outtextxycz(960-16*len(title$),1080-64,title$,154,147,4,2)

v.outtextxycz(960-16*len(shortfiles$(0)),pos,shortfiles$(0),147,154,4,2) : pos+=32
for i=1 to filenum-1: v.outtextxycz(960-16*len(shortfiles$(i)),pos,shortfiles$(i),154,147,4,2) : pos+=32 : next i

let filepos=0
do: loop until rm.readkey()=0
do 
let key=rm.readkey() 
' up $51, down $50, w $77, s $73, enter $0D
if key=$50 orelse key=$73 then
  key=0
  v.outtextxycz(960-16*len(shortfiles$(filepos)),96+32*filepos,shortfiles$(filepos),154,147,4,2) 
  filepos+=1
  if filepos>=filenum-1 then filepos=filenum-1
  v.outtextxycz(960-16*len(shortfiles$(filepos)),96+32*filepos,shortfiles$(filepos),147,154,4,2) 
endif  

if key=$51 orelse key=$77 then
  key=0
  v.outtextxycz(960-16*len(shortfiles$(filepos)),96+32*filepos,shortfiles$(filepos),154,147,4,2) 
  filepos-=1
  if filepos<0 then filepos=0
  v.outtextxycz(960-16*len(shortfiles$(filepos)),96+32*filepos,shortfiles$(filepos),147,154,4,2) 
endif  

if key=$0D then
let title$="Loading: " + files$(filepos)
v.box(0,1080-64,1919,1080-33,147)
v.outtextxycz(960-16*len(title$),1080-64,title$,154,147,4,2)
key=0

open files$(filepos) for input as #9
let pos=1: let r=0 : let psramptr=0
do
  get #9,pos,filebuf(0),16384,r : pos+=r	
  psram.write(addr(filebuf(0)),psramptr,16384)	
  psramptr+=r 					                                         ' move the buffer to the RAM and update RAM position. Todo: this can be done all at once
loop until r<>16384 '                          					         ' do until eof 
chain "/sd/test.bin"
cpustop(retrocog)
cpustop(videocog)
let loacingcog=cpu(@loadcog,@filebuf)
cpustop(cpuid())

'' todo here: stop all cogs except itself and psram
'' init a load cog
'' shut down itself

'' the load cog
endif  


loop



function addr(byref v as const any) as ulong

return(cast(ulong,@v))
end function

asm shared

             	org
loadcog      	rdlong  mailbox, ptra                        ' read pointers
                cogid   t1              		   ' get a cogid
                mul     t1, #12                            ' compute the offset to PSRAM mailbox 
                add     mailbox, t1                        ' add offset to find this COG's mailbox
                
                
                
                
                mov    hubaddr,#0
                mov    psramaddr,#0
                

p101                mov     buf1,hubaddr
                mov     buf2,##16384
                mov     cmd,psramaddr                      ' set the address
                setnib  cmd, #%1011, #7                    ' attach the command - read burst from the external memory
                setq    #2				   ' write 3 longs to the mailbox
                wrlong  cmd,mailbox			   ' read the PSRAM
poll1           rdlong  cmd, mailbox                ' poll mailbox for result
                tjs     cmd, #poll1                 ' retry until valid 

                add psramaddr,##16384
                add hubaddr, ##16384

		cmp hubaddr,##$7C000 wcz
	if_lt	jmp #p101
		hubset ##%0000_0001_0000_0000_0000_0000_1111_1000
		waitx	##200000
                cogid t1
                coginit #0,#0
                cogstop t1

' this cog will load data to the hub
t1 long 0
mailbox long $7F000
psramaddr long 0
hubaddr long 0
cmd             long    0
buf1            long    0
buf2            long    1024
end asm
