(* from batteries *)
let undefined ?(message="Undefined") _ = failwith message


(* Range *)
let (--) i j = 
  let rec aux n acc =
    if n < i then acc else aux (n-1) (n :: acc)
  in aux j []


module List = struct

    include List
              
    (* from python *)
    let all = fold_left (&&) true
                        
    (* from python *)
    let any = fold_left (||) false

    (* from stackoverflow *)
    let cartesian l l' = 
      List.concat (List.map (fun e -> List.map (fun e' -> (e,e')) l') l)
                        
  end


module String = struct

    include String

    (* from batteries *)
    let to_list s = 
      let rec exp i l =
        if i < 0 then l else exp (i - 1) (s.[i] :: l) in
      exp (String.length s - 1) []

    (* from batteries *)
    let of_list l =
      let res = Bytes.create (List.length l) in
      let rec imp i = function
        | [] -> res
        | c :: l -> res.[i] <- c; imp (i + 1) l in
      imp 0 l |> Bytes.to_string

    let split_whitespace s =
      Str.split (Str.regexp "[ \t]+") s

  end
