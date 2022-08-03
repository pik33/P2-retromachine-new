const _clkfreq=337_000_000
dim mp3 as class using "minimp3a.c"
const HEAPSIZE=128000
dim buf as ubyte(4095)
dim out as short(4095)

mount "/sd", _vfs_open_sdcard()
let filename$="/sd/test.mp3"


class mp3_info
    dim sample_rate as integer
    dim channels as integer
    dim audio_bytes as integer
end class

dim info as mp3_info

dim r as ulong

dim mp3decoder as ulong pointer 
'let mp3decoder=mp3.mp3_create()
'print mp3decoder

open filename$ for input as #8
let pos=$C15
print pos
get #8,pos,buf(0),4096,r
let t=getct()
let count=mp3.mp3_decode(mp3decoder, @buf(0), 2048, @out(0), @info)
let t=getct()-t
print t/337
print r,count
pos+=count
get #8,pos,buf(0),4096,r
let t=getct()
let count=mp3.mp3_decode(mp3decoder, @buf(0), 2048, @out(0), @info)
let t=getct()-t
print t /337
print r,count
print info.sample_rate
print info.channels
print info.audio_bytes
