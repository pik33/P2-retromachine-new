
const _clkfreq=331776000
const HEAPSIZE=65536
dim mp3  as class using "mp3.c"
dim audio as class using "audio.spin2"
'DECLARE FUNCTION mp3init LIB "mp3.c" () AS integer
'DECLARE FUNCTION mp3decode1 LIB "mp3.c" (a as any pointer, b as any pointer, c as any pointer) AS integer
dim left as integer
dim rawdata1(9000) as ubyte 
dim outbuf1(32768) as short
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
    lpoke base+16,4608*4                                    					' loop end, 
    dpoke base+20,15000                                                                        ' set volume 
    dpoke base+22,16384                                                              		' set pan
    dpoke base+24,28					                			' set period
    dpoke base+26,256 									' set skip, 1 stereo sample=4 bytes
    lpoke base+28,$0000_0000

    lpoke base+32+12,0                 								' loop start   
    lpoke base+32+16,4608*4                                      				' loop end
    dpoke base+32+20,15000                                                                      ' volume
    dpoke base+32+22,0     	                                                                ' pan
    dpoke base+32+24, 28                                                                       ' period
    dpoke base+32+26, 256									' skip
    lpoke base+32+28,$0000_0000
    
    lpoke base+8, $d0000000  +varptr(outbuf1(0))							  	        ' sample ptr, 16 bit, restart from 0 
    lpoke base+32+8, $f0000002	+varptr(outbuf1(0))						                ' sample ptr+2 (=another channel), synchronize #1 to #2'
'do: filepos+=1:bytemove(rawdata1(0),rawdata1(1),4096):loop until rawdata1(0)=$FF andalso rawdata1(1)=$FB


let filenum=1
160 let filename$="/sd/"+str$(filenum)+".mp3/": print filename$
close #7: open filename$ for input as #7	
for i=0 to 16383: outbuf1(i)=0: next i
let filepos=1
get #7,filepos,rawdata1(0),8192,q 
print filepos,q, hex$(rawdata1(0),2), hex$(rawdata1(1),2) 
 
prawdata=@rawdata1
prawdata2=prawdata
left=8192

print mp3rec

'for i=0 to 4607: outbuf1(i)=round(16000*sin((3.1415926/2304)*2*i)): next i


do
'print lpeek(base)


do: loop until lpeek(base)>768*1152

let err=mp3.mp3decode(@prawdata, @Left, @outbuf1(0)) 
let err=mp3.mp3decode(@prawdata, @Left, @outbuf1(2304)) 
let err=mp3.mp3decode(@prawdata, @Left, @outbuf1(4608)) 
let err=mp3.mp3decode(@prawdata, @Left, @outbuf1(6912)) 



let a=8192-left: filepos+=a
get #7,filepos,rawdata1(0),8192,q
prawdata=prawdata2
left=8192
if q<2048 then 
  close #7: filenum+=1
  if filenum > 20 then filenum=1
  goto 160
  endif
loop
'do: loop until lpeek(base)>512*1152

'let err=mp3.mp3decode1(@prawdata, @Left, @outbuf1(2304)) 

'let a=4096-left: filepos+=a
'get #7,filepos,rawdata1(0),1280,q
'prawdata=prawdata2
'left=4096

'do: loop until lpeek(base)>768*1152

'let err=mp3.mp3decode1(@prawdata, @Left, @outbuf1(4608)) 

'let a=4096-left: filepos+=a
'get #7,filepos,rawdata1(0),1280,q
'prawdata=prawdata2
'left=4096


'do: loop until lpeek(base)<1152*256

'let err=mp3.mp3decode1(@prawdata, @Left, @outbuf1(6912))

'let a=4096-left: filepos+=a
'get #7,filepos,rawdata1(0),1280,q
'prawdata=prawdata2
'left=4096

 













''	ERR_MP3_NONE =                  0,'
'	ERR_MP3_INDATA_UNDERFLOW =     -1,
'	ERR_MP3_MAINDATA_UNDERFLOW =   -2,
'	ERR_MP3_FREE_BITRATE_SYNC =    -3,
'	ERR_MP3_OUT_OF_MEMORY =	       -4,
'	ERR_MP3_NULL_POINTER =         -5,
'	ERR_MP3_INVALID_FRAMEHEADER =  -6,
'	ERR_MP3_INVALID_SIDEINFO =     -7,
'	ERR_MP3_INVALID_SCALEFACT =    -8,
'	ERR_MP3_INVALID_HUFFCODES =    -9,
'	ERR_MP3_INVALID_DEQUANTIZE =   -10,
'	ERR_MP3_INVALID_IMDCT =        -11,
'	ERR_MP3_INVALID_SUBBAND =      -12,

'	ERR_UNKNOWN =                  -9999


'typedef struct _MP3DecInfo {'
'	/* pointers to platform-specific data structures */
'	void *FrameHeaderPS;      0
'	void *SideInfoPS;  4
'	void *ScaleFactorInfoPS;8
'	void *HuffmanInfoPS;12
'	void *DequantInfoPS;16
'	void *IMDCTInfoPS;20
'	void *SubbandInfoPS;24
'
'	/* buffer which must be large enough to hold largest possible main_data section */
'	unsigned char mainBuf[MAINBUF_SIZE]; 28,1940
'
'	/* special info for "free" bitrate files */
'	int freeBitrateFlag;1968
'	int freeBitrateSlots;1972
'
'	/* user-accessible info */
'	int bitrate;1976
'	int nChans;1980
'	int samprate;1984
'	int nGrans;				/* granules per frame */
'	int nGranSamps;	1992		/* samples per granule */
'	int nSlots;
'	int layer;2000
'	MPEGVersion version;2004
'
'	int mainDataBegin;2008
'	int mainDataBytes;2012
'
'	int part23Length[MAX_NGRAN][MAX_NCHAN];
'
'for i=1 to 2303: print outbuf1(i), : next i
