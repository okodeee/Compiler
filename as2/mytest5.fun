fun loop(x:int):int = 
   let a = ref (x:int) in
     ((while not((!a) < 701) do a:=(!a)-1); !a)


