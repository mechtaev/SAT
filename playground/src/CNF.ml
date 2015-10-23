open LibExt


module Literal = struct

    type t = Letter of Letter.t
           | Negation of Letter.t

    let compare = Pervasives.compare

    let of_sentence = function
      | Sentence.Letter l -> Letter l
      | Sentence.Negation (Sentence.Letter l) -> Negation l
      | _ -> failwith "Literal must be a letter or its negation"

    let to_sentence = function
      | Letter l -> Sentence.Letter l
      | Negation l -> Sentence.Negation (Sentence.Letter l)

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


(* Assume that sentence is in CNF *)
let of_sentence sentence =
  sentence |> Sentence.to_conjuncts
           |> List.map Sentence.to_disjuncts
           |> List.map @@ List.map @@ Literal.of_sentence
           |> List.map Clause.of_list


let to_sentence cnf =
  cnf |> List.map Clause.elements
      |> List.map @@ List.map @@ Literal.to_sentence
      |> List.map Sentence.of_disjuncts
      |> Sentence.of_conjuncts


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
