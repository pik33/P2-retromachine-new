const _clkfreq = 336956522
'const _clkfreq=331776000
const HEAPSIZE=8192
dim newstack(1024) as ubyte
dim mp3  as class using "mp3.c"
dim audio as class using "audio.spin2"

 const version$="Prop2play v.0.30"
const statusline$=" Propeller2 multiformat player v. 0.30 --- 2023.05.09 --- pik33@o2.pl --- use an USB keyboard and mouse to control --- arrows up,down move - pgup/pgdn or w/s move 10 positions - enter selects - tab switches panels - +,- controls volume - 1..4 switch channels on/off - 5,6 stereo separation - 7,8,9 sample rate - a,d SID speed - x,z SID subtune - R rescans current directory ------"


dim v as class using "hg008.spin2"
dim rm as class using "usbnew.spin2"
dim tracker as class using "trackerplayer.spin2"
dim paula as class using "audio093b-8-sc.spin2"
dim sid as class using "sidcog8.spin2"
dim psram as class using "psram.spin2"
dim spc as class using "spccog.spin2"
dim a6502 as class using "a6502-1.spin2"
dim mp3 as class using "mp3.c"
#include "dir.bi"


dim newdl(32)
dim audiocog,videocog as integer
dim base as ulong
dim mbox as ulong

sub startpsram
psram.startx(0, 0, 11, -1)
mbox=psram.getMailbox(0)
end sub

sub startaudio
audiocog,base=paula.start(mbox,$75A00,$7A400)
end sub 

sub stopaudio
cpustop(audiocog)
audiocog=-1
end sub

sub cls(fg=154,bg=147)
v.cls(fg,bg)
end sub

function startvideo(mode=64, pin=0, mb=0) 'todo return a cog#
dim videocog as ulong
videocog=v.start(pin,mbox)

for thecog=0 to 7:psram.setQos(thecog, 112 << 16) :next thecog
psram.setQoS(videocog, $7FFFf400) 
open SendRecvDevice(@v.putchar, nil, nil) as #0
return videocog
end function

#define plot v.plot1

sub pslpoke(addr as ulong,value as ulong)
psram.filllongs(addr,value,1,0)
end sub

sub pspoke(addr as ulong,value as ulong)
psram.fillbytes(addr,value,1,0)
end sub

function pspeek(adr as ulong) as ubyte
dim res as ubyte
psram.read1(addr(res),adr,1)
return res
end function

function pslpeek(adr as ulong) as ulong
dim res as ulong
psram.read1(addr(res),adr,4)
return res
end function

function addr(byref v as const any) as ulong

return(cast(ulong,@v))
end function

sub position(x,y)
v.setcursorpos(x,y)
end sub

sub waitvbl
  v.waitvbl(1)
end sub

rm.start()
rm.mouse_set_limits(1023,767)
startpsram
startvideo
cls(154,147)
makedl
preparepanels

'DECLARE FUNCTION mp3init LIB "mp3.c" () AS integer
'DECLARE FUNCTION mp3decode1 LIB "mp3.c" (a as any pointer, b as any pointer, c as any pointer) AS integer
dim left,err as integer
dim rawdata1(8191) as ubyte 
dim outbuf1(9215) as short
dim q as integer
dim prawdata,prawdata2 as ubyte pointer
mount "/sd", _vfs_open_sdcard()
chdir "/sd"
dim err as integer
let mp3rec=mp3.mp3init()

dim cog,base as ulong
let cog, base=audio.start(0,0,0)
waitms(2)
 lpoke base+28,$80000100 : waitms(2) : lpoke base+28,$00000000  
    lpoke base+12,0               								' loop start   
    lpoke base+16,4608*4      -4                              					' loop end, 
    dpoke base+20,15000                                                                        ' set volume 
    dpoke base+22,16384                                                              		' set pan
    dpoke base+24,28					                			' set period
    dpoke base+26,256 									' set skip, 1 stereo sample=4 bytes
    lpoke base+28,$0000_0000

    lpoke base+32+12,0                 								' loop start   
    lpoke base+32+16,4608*4 -4                                     				' loop end
    dpoke base+32+20,15000                                                                      ' volume
    dpoke base+32+22,0     	                                                                ' pan
    dpoke base+32+24, 28                                                                       ' period
    dpoke base+32+26, 256									' skip
    lpoke base+32+28,$0000_0000
    
    lpoke base+8, $d0000000  +varptr(outbuf1(0))							  	        ' sample ptr, 16 bit, restart from 0 
    lpoke base+32+8, $f0000002	+varptr(outbuf1(0))						                ' sample ptr+2 (=another channel), synchronize #1 to #2'
'do: filepos+=1:bytemove(rawdata1(0),rawdata1(1),4096):loop until rawdata1(0)=$FF andalso rawdata1(1)=$FB


let filenum=2
let cog=cpu(newcog(),@newstack(1))
160 let filename$="/sd/"+str$(filenum)+".mp3/": print filename$
close #7: open filename$ for input as #7	
for i=0 to 9214: outbuf1(i)=0: next i
let filepos=1
get #7,filepos,rawdata1(0),6144,q 
print filepos,q, hex$(rawdata1(0),2), hex$(rawdata1(1),2) 
 
prawdata=@rawdata1
prawdata2=prawdata
left=6144

print mp3rec




do



do: loop until err=1 


err=0


let a=6144-left: filepos+=a
get #7,filepos,rawdata1(0),6144,q
prawdata=prawdata2
left=6144
if q<2048 then 
  close #7: filenum+=1
  if filenum > 20 then filenum=1
  goto 160
  endif
loop



sub newcog()

do
do: loop until lpeek(base)>768*1152

let err=mp3.mp3decode(@prawdata, @Left, @outbuf1(0))  
let err=mp3.mp3decode(@prawdata, @Left, @outbuf1(2304))  
let err=mp3.mp3decode(@prawdata, @Left, @outbuf1(4608))  
let err=mp3.mp3decode(@prawdata, @Left, @outbuf1(6912)) 
err=1
loop 

end sub


sub makedl
newdl(0)=559<<20+(0)<<16+%0001+ (0+(v.cpl1<<2)) <<4             
newdl(1)=v.buf_ptr<<4+%10  
for i=2 to 17: newdl(i)=$7000002+2*65536*(i-2) :next i
for i=18 to 32: newdl(i)=$7000002 : next i
v.dl_ptr=addr(newdl(0))
end sub


' ---------------- Prepare the user interface --- rev 20220206 ---------------------------------------------------

sub preparepanels

' 1. Channel and oscilloscope panel at graphic canvas

cls(154,114)	
v.setfontfamily(4)						'
v.outtextxycz((1024-32*len(version$))/2,4,version$,120,114,4,2)
v.setfontfamily(0)						'

v.frame(4,408,524,555,15)							' clear the panel
v.box(5,409,523,427,188)							' clear the panel
v.box(5,428,523,554,177)							' clear the panel
v.outtextxycf(12,410,"Osciloscope",0)

v.frame(528,408,720,555,15)							' clear the panel
v.box(529,409,719,427,26)							' clear the panel
v.box(529,428,719,554,16)							' clear the panel
v.outtextxycf(536,410,"Visualization",0)

v.frame(724,408,1019,555,15)							' clear the panel
v.box(725,409,1018,427,170)							' clear the panel
v.box(725,428,1018,554,160)
v.outtextxycf(732,410,"Channels",0)

v.frame(724,40,1019,404,15)							' clear the panel
v.box(725,41,1018,59,154)
v.box(725,60,1018,403,147)
v.outtextxycf(732,43,"File info",0)

v.frame(362,40,720,404,15)							' clear the panel
v.box(363,41,719,59,40)							' clear the panel
v.box(363,60,719,403,34)							' clear the panel
v.outtextxycf(370,43,"Files",0)

v.frame(4,40,358,236,15)							' clear the panel
v.box(5,41,357,59,200)							' clear the panel
v.box(5,60,357,235,193)							' clear the panel
v.outtextxycf(12,43,"Directories",0)

v.frame(4,240,358,324,15)							' clear the panel
v.box(5,241,357,259,232)							' clear the panel
v.box(5,260,357,323,225)							' clear the panel
v.outtextxycf(12,243,"Now playing",0)

v.frame(4,328,358,404,15)							' clear the panel
v.box(5,329,357,347,122)							' clear the panel
v.box(5,348,357,403,114)							' clear the panel
v.outtextxycf(12,331,"Status",0)

let oldcpl=v.s_cpl: let oldcpl1=v.s_cpl1: let oldbufptr=v.s_buf_ptr: v.setfontfamily(4)
v.s_cpl=2048:v.s_cpl1=2048: v.s_buf_ptr=$700000: :position 0,0 : v.outtextxycg(0,0,statusline$+statusline$,120,114)
v.s_cpl=oldcpl:v.s_cpl1=oldcpl1:v.s_buf_ptr=oldbufptr: v.setfontfamily(0)


end sub
