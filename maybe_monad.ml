module Maybe_monad = struct
  type 'a t = 'a option
  let return x = Some(x)
  let (>>=) m f =
  match m with
  | None -> None
  | Some x -> f x
  
  let (/) (x: int option) (y: int option) =
    x >>= fun a ->
    y >>= fun b -> if b = 0 then None else Some(Stdlib.(/) a b)

  let () = 
    let x = ((return 30 / return 4) / return (10 - 10)) / return 2 in
    match x with
    | Some a -> print_endline (string_of_int a)
    | None -> print_endline "division by 0"

end
