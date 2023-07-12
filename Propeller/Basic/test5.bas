class a
  dim b as integer
  dim c as integer
  dim d as integer
  dim e as integer
end class
 
print f().b

function f() as a

dim g as a

g.b=23456
print "Returned: "; h().b
g = h()
print "Assigned:"; g.b
return g    
end function

function h() as a

dim i as a
i.b=2
return i
end function
