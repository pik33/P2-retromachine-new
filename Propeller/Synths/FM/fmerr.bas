dim base,min,minc as integer


function addr(byref v as const any) as ulong
return(cast(ulong,@v))
end function


base=addr(abss)+272*16: 






  





asm shared
abss		file "h/abs.h2"		'0	
default		file "h/default.h2"	'1
dimx		file "h/dimx.h2"	'2
 
harmon		file "s/harmon.s2"	'10

otto1		file "s/otto1.s2"	'22
pila		file "s/pila.s2"	'23
pila1		file "s/pila1.s2"	'24
'45


'sinus file "s/sinus.s2"
end asm


asm shared

cordic	org 0
       
p101   getct   time1
       qrotate a1000,angle1
       add angle1,delta1
       qrotate a1000,angle2
       add angle2,delta2
       qrotate a1000,angle3
       add angle3,delta3
       qrotate a1000,angle4
       add angle4,delta4
       qrotate a1000,angle5
       add angle5,delta5
       qrotate a1000,angle6
       add angle6,delta6
       getqx result1
       qrotate a1000,angle1
       add angle1,delta1       
       getqx result2
       qrotate a1000,angle2
       add angle2,delta2      
       getqx result3
       qrotate a1000,angle3
       add angle3,delta3       
       getqx result4
       qrotate a1000,angle4
       add angle4,delta4       
       getqx result5
       qrotate a1000,angle5
       add angle5,delta5       
       getqx result6
       qrotate a1000,angle6
       add angle6,delta6         
       getqx result11
       getqx result12
       getqx result13
       getqx result14
       getqx result15
       getqx result16
       getct time2
       sub time2,time1
       
       wrlong result1,#$30
       wrlong result2,#$34
       wrlong result3,#$38
       wrlong result4,#$3c
       wrlong result5,#$40
       wrlong result6,#$44
       wrlong result11,#$48
       wrlong result12,#$4c
       wrlong result13,#$50
       wrlong result14,#$54
       wrlong result15,#$58
       wrlong result16,#$5c
       wrlong time2,#$60
       waitx ##2_999_999       

       jmp #p101
       
a1000 long $10000
angle1 long 0
angle2 long 0
angle3 long 0
angle4 long 0
angle5 long 0
angle6 long 0
delta1 long $00800000
delta2 long $00900000
delta3 long $00a00000
delta4 long $00b00000
delta5 long $00c00000
delta6 long $00d00000
result1 long 0
result2 long 0
result3 long 0
result4 long 0
result5 long 0
result6 long 0
result11 long 0
result12 long 0
result13 long 0
result14 long 0
result15 long 0
result16 long 0
       
time1 long 0
time2 long 0

end asm
