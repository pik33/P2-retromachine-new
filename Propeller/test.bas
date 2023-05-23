

function silnia(n) as integer
if n=1 orelse n=0 then return 1 else return n*silnia(n-1)
end function

for i=1 to 10: print silnia(i): next i
