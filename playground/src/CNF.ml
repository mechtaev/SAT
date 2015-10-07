open LibExt

module Literal = struct

    type t = Letter of Form.Id.t
           | Negation of Form.Id.t

    let compare = Pervasives.compare

    let of_form = function
      | Form.Letter id -> Letter id
      | Form.Negation (Form.Letter id) -> Negation id
      | _ -> failwith "Literal must be a letter or its negation"

    let to_form = function
      | Letter id -> Form.Letter id
      | Negation id -> Form.Negation (Form.Letter id)

  end

module LiteralSet = Set.Make(Literal)

type t = LiteralSet.t list

(* Assume that form is in CNF *)
let of_form form =
  form |> Form.to_conjuncts
       |> (List.map Form.to_disjuncts)
       |> (List.map @@ List.map @@ Literal.of_form)
       |> (List.map LiteralSet.of_list)

let to_form cnf =
  cnf |> (List.map LiteralSet.elements)
      |> (List.map @@ List.map @@ Literal.to_form)
      |> (List.map Form.of_disjuncts)
      |> Form.of_conjuncts
