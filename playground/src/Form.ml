open LibExt

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
  let to_string = function
    | Letter id     -> (fun _ -> Id.to_string id)
    | Negation _    -> (function [x]    -> Printf.sprintf "~%s" x)
    | Conjunction _ -> (function [x; y] -> Printf.sprintf "(%s & %s)" x y)
    | Disjunction _ -> (function [x; y] -> Printf.sprintf "(%s | %s)" x y)
  in
  form |> to_conjuncts |> (List.map (fun form -> fold to_string form))
