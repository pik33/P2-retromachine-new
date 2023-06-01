sub a()
 
for i=1 to 20
b()
print i
next i
end sub

sub b()
let i=i+1
end sub

a()
