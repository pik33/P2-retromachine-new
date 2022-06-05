const _clkfreq=338000000

dim r(255) as ubyte

let e2=0
for i=$8000 to $7FF00 step $100
let e1=0 
for k=0 to 20
for j=0 to 255
r(j)=getrnd() mod 256
next j
for j=0 to 255: poke i+j,r(j): next j
for j=0 to 255: if peek(i+j)<>r(j) then e1=1
 next j
 next k
 
e2+=e1
print i
next i
print e2

function peek(addr) as ubyte
dim r as ubyte
asm
rdbyte r,addr
end asm
return r
end function

sub poke(addr as ulong,value as ubyte)
asm
wrbyte value, addr
end asm
end sub
