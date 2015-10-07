
let not_implemented: 'a. unit -> 'a =
  fun _ ->
  failwith "Not implemented";
  Obj.magic ()

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
      let res = String.create (List.length l) in
      let rec imp i = function
        | [] -> res
        | c :: l -> res.[i] <- c; imp (i + 1) l in
      imp 0 l

    let split_whitespace s =
      Str.split (Str.regexp "[ \t]+") s

  end
