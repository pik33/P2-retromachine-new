dim mp3 as class using "minimp3.c"

dim buf as ubyte(2048)
dim out as ubyte(2048)

mount "/sd", _vfs_open_sdcard()
chdir "/sd/mp3/SKiSK"
let currentdir$="/sd/mp3/SKiSK/"
let filename$="/sd/mp3/SKiSK/00 Zlota medytacja.mp3"


dim r as ulong

dim mp3decoder as ulong pointer
mp3decoder=mp3.mp3_create()

open filename$ for input as #8
let pos=1
get #8,pos,buf(0),2048,r
let count=mp3.mp3_decode(mp3decoder, nil, 2048, nil, nil)
