union eresult
  dim iresult as integer
  dim uresult as ulong
  dim lresult as longint
  dim ulresult as ulongint
  dim fresult as double
  dim sresult as string
  end union

class expr_result
dim aresult as eresult
dim result_type as ubyte  
end class

class part
  dim part$ as string
  dim token as integer
  dim priority as integer
end class

type parts as part(125) 
dim lparts as parts
dim ct as integer
dim t1 as expr_result

const token_decimal=512

'---------------------------------------
print "Expected result=2, result type=1"
print

lparts(0).part$="2"
lparts(0).token=token_decimal
ct=0

t1=expr()
print "Expression result="; t1.aresult.uresult
print "Result type=";t1.result_type

'-------------------------------------

function expr() as expr_result

dim t2 as expr_result

t2 = muldiv()   
print "in expr: result=";t2.aresult.uresult
print "in expr: result type=";t2.result_type
print
return t2
end function


function muldiv() as expr_result

dim t3 as expr_result

t3 = getvalue()    
print "in muldiv: result=";t3.aresult.uresult
print "in muldiv: result type=";t3.result_type
print
return t3    
end function

function getvalue() as expr_result

dim t4 as expr_result
dim op as integer

op=lparts(ct).token

select case op
  case token_decimal
    t4.aresult.uresult=val%(lparts(ct).part$): t4.result_type=1 ' todo token_int64
    print "in getvalue: result=";t4.aresult.uresult
    print "in getvalue: result type=";t4.result_type
    print
  end select  
return t4
end function
