
var b=0
var c=0
for a=$00000000 to $7f000000 step $1000000

 

asm
   qexp a
   getqx b
   mov c,a
   shl c,#1
   qexp c
   getqx c
end asm

print hex$(a), hex$(b), hex$(c)   

next a
