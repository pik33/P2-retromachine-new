dim psram as class using "psram4"

dim s(512)

psram.startx(0, 0, 12, -1)
let mbox=psram.getMailbox(0)

dim list1(59)

let p=0
for i=0 to 13
let amount=getrnd() mod 64

list1(4*i+0)=$B0_600000+p					 
list1(4*i+1)=$60000+p
list1(4*i+2)=amount
list1(4*i+3)=addr(list1(4*i+4))
p=p+amount
next i
i=14
list1(4*i+0)=$B0_600000+p					 
list1(4*i+1)=$60000+p
list1(4*i+2)=1024-p
list1(4*i+3)=0





'cog1
let cog=cpu(cog1,@s)

sub cog1
'print cpuid()
for thecog=1 to 7:psram.setQos(thecog, 0) :next thecog : waitms 1000
psram.setQoS(cpuid(), $7FFFF400) 
waitms(1000)

lpoke 12*cpuid()+mbox+4,addr(list1)
lpoke 12*cpuid()+mbox,$FFFFFFFF
let t1=getct()
do : loop until (lpeek(12*cpuid()+mbox) and $80000000) = 0
let t1=getct()-t1: print t1

lpoke 12*cpuid()+mbox+8,1024
lpoke 12*cpuid()+mbox+4,$60000
lpoke 12*cpuid()+mbox,$B060_0000
let t1=getct()
do : loop until (lpeek(12*cpuid()+mbox) and $80000000) = 0
let t1=getct()-t1: print t1

t1=getct()
for i=1 to 15
lpoke 12*cpuid()+mbox+8,64
lpoke 12*cpuid()+mbox+4,$60000
lpoke 12*cpuid()+mbox,$B060_0000  
do : loop until (lpeek(12*cpuid()+mbox) and $80000000) = 0
next i
t1=getct()-t1: print t1

end sub

sub lpoke(addr as ulong,value as ulong)
asm
wrlong value, addr
end asm
end sub

function addr(byref v as const any) as ulong

return(cast(ulong,@v))
end function

function lpeek(addr) as ulong
dim r as ulong
asm
rdlong r,addr
end asm
return r
end function
