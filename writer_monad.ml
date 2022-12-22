module Writer_monad = struct
  type 'a t = ('a * string)
  let return x = (x, "")
  let (>>=) m f =  
    let (x, s1) = m in
    let (y, s2) = f x in
    (y, s1 ^ s2)
  
  let log_inc x =
    x >>= fun a -> (a + 1, Printf.sprintf "apply inc %d -> %d; " a (a+1))

   let log_dec x =
    x >>= fun a -> (a - 1, Printf.sprintf "apply dec %d -> %d; " a (a-1))
  
  let () = 
    let a = return 5 in
    let b = log_inc a in
    let c = log_inc b in
    let d = log_dec c in
    let e = log_inc d in
    let f, g = log_inc e in
    print_endline g

end
