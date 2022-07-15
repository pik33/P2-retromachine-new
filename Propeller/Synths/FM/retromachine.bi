const _clkfreq = 336956522

dim v as class using "hng050a.spin2"
dim rm as class using "midicog.spin2"
dim audio as class using "audio-fm008.spin2"
#include "dir.bi"

dim audiocog as integer
dim base as ulong
dim mbox as ulong

sub startaudio
audiocog,base=audio.start()
end sub 

function startvideo(mode=512+48, pin=0, mb=0) 'todo return a cog#
dim videocog as ulong
videocog=v.start(mode,pin,mbox)
v.setbordercolor(0,0,0)
v.cls(154,147)
open SendRecvDevice(@v.putchar, nil, nil) as #0
return videocog
end function

#define startmidi rm.start

function peek(addr) as ubyte
dim r as ubyte
asm
rdbyte r,addr
end asm
return r
end function

function dpeek(addr) as ushort
dim r as ushort
asm
rdword r,addr
end asm
return r
end function

function lpeek(addr) as ulong
dim r as ulong
asm
rdlong r,addr
end asm
return r
end function

function slpeek(addr) as integer
dim r as integer
asm
rdlong r,addr
end asm
return r
end function

sub poke(addr as ulong,value as ubyte)
asm
wrbyte value, addr
end asm
end sub

sub dpoke(addr as ulong,value as ushort)
asm
wrword value, addr
end asm
end sub

sub lpoke(addr as ulong,value as ulong)
asm
wrlong value, addr
end asm
end sub

function addr(byref v as const any) as ulong
return(cast(ulong,@v))
end function

sub position(x,y)
v.setcursorpos(x,y)
end sub

sub cls(fg=154,bg=147)
v.cls(fg,bg)
end sub
