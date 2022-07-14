let slider1=0
let slider2=0
let slider3=0
let slider4=0

let  r1=round(61*exp(slider1/10.0)) 
let  r2=round(61*exp(slider2/10.0))
let  r3=round(61*exp(slider3/10.0))
let  r4=round(61*exp(slider4/10.0)) 

print slider1,hex$(r1,8),slider4,hex$(r4,8)

var aa=16384+$FFFF0000
   asm
    scas aa,##16384
    mov  aa,0-0
    end asm
    
    print aa
