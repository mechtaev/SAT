(* Range *)
let (--) i j = 
  let rec aux n acc =
    if n < i then acc else aux (n-1) (n :: acc)
  in aux j []

let all list =
  List.fold_left (&&) true list

let any list =
  List.fold_left (||) false list
