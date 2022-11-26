const _clkfreq = 336956522

dim v as class using "hng050.spin2"
dim rm as class using "midicog.spin2"
dim audio as class using "audio-as003.spin2"
dim flash as class using "jm_p2_flash.spin2"
dim psram as class using "psram.spin2"
#include "dir.bi"


dim audiocog as integer
dim base as ulong
dim mbox as ulong

sub startaudio
audiocog,base=audio.start()
end sub 

function startvideo(mode=0, pin=0, mb=0) 'todo return a cog#
dim videocog as ulong
videocog=v.start(pin,mode,mb)
v.setbordercolor(0,0,0)
v.cls(154,147)
'for thecog=0 to 7:psram.setQos(thecog, 112 << 16) :next thecog
psram.setQoS(videocog, $7FFFf400) 
open SendRecvDevice(@v.putchar, nil, nil) as #0
return videocog
end function

sub startpsram
psram.startx(0, 0, 11, -1)
mbox=psram.getMailbox(0)
end sub

#define startmidi rm.start
#define addr varptr

sub position(x,y)
v.setcursorpos(x,y)
end sub

sub cls(fg=154,bg=147)
v.cls(fg,bg)
end sub

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
