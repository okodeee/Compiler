fun fact(x:int ref):int = 
  if (!x) < 1 then 1
    else let a = !x in
         let b = dec_ref(x) in
           a * fact(x)

