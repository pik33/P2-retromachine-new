DECLARE FUNCTION mp3init LIB "mp3b.c" () AS integer

dim rawdata1(4096) as ubyte 

dim q as integer
mount "/sd", _vfs_open_sdcard()
chdir "/sd"
close #7: open "/sd/test.mp3/" for input as #7	

get #7,$415,rawdata1(0),4096,q 
close #7
print q, hex$(rawdata1(0),2), hex$(rawdata1(1),2)  


dim err as integer
let err=mp3init()
