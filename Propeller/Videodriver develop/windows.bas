class TWindow
     dim handle as TWindow pointer
     dim aprev,anext as TWindow  pointer               
     dim x,y,l,h as integer                      
     dim vcl,vch as integer                    
     dim vcx,vcy as integer                  
     dim canvas as ulong
     dim needclose,selected as ubyte
end class 

class psram_memblock
  dim start as ulong
  dim aend as ulong
  dim aprev as psram_memblock pointer
  dim anext as psram_memblock pointer
  dim afree as ubyte
  
  sub create(s,e as ulong)
    start=s
    aend=e
    aprev=nil
    anext=nil
    afree=1
  end  
end class

class psram_mem
  dim start as ulong
  dim aend as ulong  
  dim afirst as psram_memblock pointer
  
  sub create
    start=0
    aend=$6FFFFF
    afirst=nil
  end sub
  
  function allocate(amount) as integer 
  
  if afirst=nil
    if amount>700000 then return -1
    
         
  
  
  
end class
