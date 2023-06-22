class expr_result
  dim uresult as integer
  dim result_type as ulong
  dim sresult as string
end class
 
'---------------------------------------
print muldiv().uresult
'-------------------------------------

function muldiv() as expr_result

dim t3 as expr_result

t3.uresult=23456
print "Returned uresult: ";getvalue().uresult
t3 = getvalue()
print "Assigned uresult:";t3.uresult
print
return t3    
end function

function getvalue() as expr_result

dim t4 as expr_result
t4.uresult=2
return t4
end function
