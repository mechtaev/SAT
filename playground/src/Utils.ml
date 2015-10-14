let print_clauses clauses =
  match clauses with
  | [] -> print_endline "<empty>"
  | _ -> List.iter
           (fun clause -> [clause] |> CNF.to_form |> Form.visualize |> print_endline)
           clauses
