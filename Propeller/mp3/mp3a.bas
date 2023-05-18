
const _clkfreq=331776000
const HEAPSIZE=4096
dim newstack(1024) as ubyte
dim mp3  as class using "mp3.c"
dim audio as class using "audio.spin2"
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
