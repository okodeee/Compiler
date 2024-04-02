fun dec_ref(x:int ref):int = 
  let a = !x in
    (x := a - 1; <>)

 
