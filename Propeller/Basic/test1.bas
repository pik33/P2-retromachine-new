class part
  dim part1$ as string
  dim part_type as integer
  dim priority as integer
end class

dim lpart as part(125)

lpart(1).part$="test"
lpart(2).part$="test1"
print lpart(1).part$
print lpart(2).part$
