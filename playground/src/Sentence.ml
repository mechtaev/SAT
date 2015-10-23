open LibExt


type t = Letter of Letter.t
       | Conjunction of t * t
       | Disjunction of t * t
       | Negation of t


module Constructors = struct

    let mk_letter () =
      Letter (Letter.mk_letter ())

    let (&&) x y = Conjunction (x, y)

    let (||) x y = Disjunction (x, y)

    let (!) x = Negation x

  end


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
  | h :: t -> Constructors.(h && (of_conjuncts t))


let rec of_disjuncts = function
  | h :: [] -> h
  | h :: t -> Constructors.(h || (of_disjuncts t))

(* list of conjuncts *)
let visualize: t -> string =
  fun form ->
  let to_string = function
    | Letter l                                   -> (fun _ -> Letter.to_string l)
    | Negation (Letter _)                        -> (function [x]    -> Printf.sprintf "¬%s" x)
    | Negation _                                 -> (function [x]    -> Printf.sprintf "~(%s)" x)
    | Conjunction (Disjunction _, Disjunction _) -> (function [x; y] -> Printf.sprintf "(%s) ∧ (%s)" x y)
    | Conjunction (Disjunction _, _)             -> (function [x; y] -> Printf.sprintf "(%s) ∧ %s" x y)
    | Conjunction (_, Disjunction _)             -> (function [x; y] -> Printf.sprintf "%s ∨ (%s)" x y)
    | Conjunction _                              -> (function [x; y] -> Printf.sprintf "%s ∨ %s" x y)
    | Disjunction _                              -> (function [x; y] -> Printf.sprintf "%s ∨ %s" x y)
  in
  fold to_string form
