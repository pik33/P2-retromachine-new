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

function do_plus(t1 as expr_result ,t2 as expr_result) as expr_result
t1.result_type=1
t1.aresult.iresult=5
return t1
end function
