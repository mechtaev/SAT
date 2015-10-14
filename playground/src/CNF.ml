open LibExt


module Literal = struct

    type t = Letter of Letter.t
           | Negation of Letter.t

    let compare = Pervasives.compare

    let of_form = function
      | Form.Letter l -> Letter l
      | Form.Negation (Form.Letter l) -> Negation l
      | _ -> failwith "Literal must be a letter or its negation"

    let to_form = function
      | Letter l -> Form.Letter l
      | Negation l -> Form.Negation (Form.Letter l)

    let negate = function
      | Letter l -> Negation l
      | Negation l -> Letter l

    let to_letter = function
      | Letter l -> l
      | Negation l -> l

  end


module Clause = Set.Make(Literal)


type t = Clause.t list


let is_unit clause =
  (Clause.cardinal clause) == 1


let mem: Literal.t -> t -> bool =
  fun lit cnf ->
  List.any (List.map (Clause.mem lit) cnf)


(* Assume that form is in CNF *)
let of_form form =
  form |> Form.to_conjuncts
       |> List.map Form.to_disjuncts
       |> List.map @@ List.map @@ Literal.of_form
       |> List.map Clause.of_list


let to_form cnf =
  cnf |> List.map Clause.elements
      |> List.map @@ List.map @@ Literal.to_form
      |> List.map Form.of_disjuncts
      |> Form.of_conjuncts


let letters: t -> Letter.t list =
  fun cnf ->
  let module LetterSet = Set.Make(Letter) in
  let letters = List.fold_left
                  LetterSet.union
                  LetterSet.empty
                  (cnf |> List.map Clause.elements
                       |> List.map @@ List.map @@ Literal.to_letter
                       |> List.map LetterSet.of_list)
  in
  LetterSet.elements letters
