open LibExt

module Id = struct

    type t = int

    let last_id = ref 0

    let mk_id () =
      let id = !last_id in
      last_id := id + 1;
      id

    let to_int id = id

    let compare = Pervasives.compare

    let equal = (=)

    let hash i = Hashtbl.hash

  end

type t = Letter of Id.t
       | Conjunction of t * t
       | Disjunction of t * t
       | Negation of t

module Constructors = struct

    let mk_letter () =
      Letter (Id.mk_id ())

    let (&.) x y = Conjunction (x, y)

    let (|.) x y = Disjunction (x, y)

    let (~.) x = Negation x

  end

open Constructors

(* bottom up left-to-right *)
let rec fold f form =
  match form with
  | Letter _           -> f form []
  | Negation x         -> f form [fold f x]
  | Conjunction (x, y) -> let left = fold f x in
                          let right = fold f y in
                          f form [left; right]
  | Disjunction (x, y) -> let left = fold f x in
                          let right = fold f y in
                          f form [left; right]

let to_conjuncts form =
  let split = function
    | Letter _ as l      -> (fun _ -> [l])
    | Conjunction _      -> (function [x; y] -> x @ y)
    | Disjunction _ as d -> (fun _ -> [d])
    | Negation _ as n    -> (fun _ -> [n])
  in
  fold split form

let to_disjuncts form =
  let split = function
    | Letter _ as l      -> (fun _ -> [l])
    | Conjunction _ as c -> (fun _ -> [c])
    | Disjunction _      -> (function [x; y] -> x @ y)
    | Negation _ as n    -> (fun _ -> [n])
  in
  fold split form

let rec of_conjuncts = function
  | h :: [] -> h
  | h :: t -> h &. (of_conjuncts t)

let rec of_disjuncts = function
  | h :: [] -> h
  | h :: t -> h |. (of_disjuncts t)

(* list of conjuncts *)
let visualize: t -> string list =
  fun form ->
  let name_of id =
    let rec to_char_list id =
      let to_char i = Char.chr ((Char.code 'a') + i) in
      let base = (Char.code 'z') - (Char.code 'a') + 1 in
      if id < base then
        [to_char id]
      else
        (to_char_list (id / base)) @ [to_char (id mod base)]
    in
    id |> to_char_list |> String.of_list
  in
  let to_string = function
    | Letter id     -> (fun _ -> name_of id)
    | Negation _    -> (function [x]    -> Printf.sprintf "~%s" x)
    | Conjunction _ -> (function [x; y] -> Printf.sprintf "(%s & %s)" x y)
    | Disjunction _ -> (function [x; y] -> Printf.sprintf "(%s | %s)" x y)
  in
  form |> to_conjuncts |> (List.map (fun form -> fold to_string form))
