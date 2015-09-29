open Utils

type var_id = int

type t = Var of var_id
       | Neg of t
       | And of t * t
       | Or of t * t
       | Xor of t * t
       | Iff of t * t

let last_id = ref 0

let mk_var () =
  let id = !last_id in
  last_id := id + 1;
  Var id

let rec fold f term =
  match term with
  | Var _   -> f term []
  | Neg t   -> f term [fold f t]
  | And (x, y) -> f term [fold f x; fold f y]
  | Or  (x, y) -> f term [fold f x; fold f y]
  | Xor (x, y) -> f term [fold f x; fold f y]
  | Iff (x, y) -> f term [fold f x; fold f y]

let visualize terms =
  let value = ref (function _ -> 'a') in
  let size = ref 1 in
  let mk_name () =
    let name = String.init !size !value in
    let rec increase_from value index =
      if value index < 'z' then
        (fun i -> if i = index then (Char.code (value i)) + 1 |> Char.chr else value i)
      else
        let new_value = (fun i -> if i = index then 'a' else value i) in
        if !size > index then
          increase_from new_value (index + 1)
        else
          (size := !size + 1;
           new_value)
    in
    value := increase_from !value 0;
    name
  in
  let var_names: (var_id, string) Hashtbl.t = Hashtbl.create 1024 in
  let to_string = function 
    | Var id ->
       (fun _ -> if Hashtbl.mem var_names id then
                   Hashtbl.find var_names id
                 else
                   let name = mk_name () in
                   Hashtbl.add var_names id name;
                   name)
    | Neg _ -> (function [t]    -> Printf.sprintf "~%s" t)
    | And _ -> (function [x; y] -> Printf.sprintf "(%s & %s)" x y)
    | Or  _ -> (function [x; y] -> Printf.sprintf "(%s | %s)" x y)
    | Xor _ -> (function [x; y] -> Printf.sprintf "(%s xor %s)" x y)
    | Iff _ -> (function [x; y] -> Printf.sprintf "(%s <=> %s)" x y)
  in
  List.map (fun term -> fold to_string term) terms
    

  
