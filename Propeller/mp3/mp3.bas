dim mp3 as class using "mp3.c"
dim rawdata(4096) as ubyte 
dim prawdata as ubyte pointer
dim outbuf(8192) as short
dim q as integer
mount "/sd", _vfs_open_sdcard()
chdir "/sd"
close #7: open "/sd/test.mp3/" for input as #7	
  
get #7,$415,rawdata(0),4096,q :
close #7

dim left as integer
prawdata=@rawdata(0)
dim err as integer
let err=mp3.mp3init()
let time=getct()
let result=mp3.mp3decode(@prawdata, @left, @outbuf(0)) 
time=getct()-time
print result,time
