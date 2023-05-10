dim mp3 as class using "mp3.c"
dim rawdata(4096) as ubyte 
dim q as integer
mount "/sd", _vfs_open_sdcard()
chdir "/sd"
close #7: open "/sd/test.mp3/" for input as #7	
print "kwas"
get #7,$415,rawdata(0),4096,q :
close #7

'/* public API */
'HMP3Decoder MP3InitDecoder(void);
'void MP3FreeDecoder(HMP3Decoder hMP3Decoder);
'int MP3Decode(HMP3Decoder hMP3Decoder, unsigned char **inbuf, int *bytesLeft, short *outbuf, int useSize);

'void MP3GetLastFrameInfo(HMP3Decoder hMP3Decoder, MP3FrameInfo *mp3FrameInfo);
'int MP3GetNextFrameInfo(HMP3Decoder hMP3Decoder, MP3FrameInfo *mp3FrameInfo, unsigned char *buf);
'int MP3FindSyncWord(unsigned char *buf, int nBytes);

dim err as any pointer
let err=mp3.mp3initdecoder()
print decuns$(lpeek(cast(ulong,@err)))
'let time=getct()
'let result=mp3.mp3dec_decode_frame(@mp3.dec, rawdata, 4096,  mp3.pcm, @mp3.info) 
' time=getct()-time
' print result,time,mp3.info.frame_bytes, mp3.info.frame_offset, mp3.info.channels, mp3.info.hz, mp3.info.layer, mp3.info.bitrate_kbps;
