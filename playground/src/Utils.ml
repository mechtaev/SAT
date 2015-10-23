let print_clauses clauses =
  match clauses with
  | [] -> print_endline "<empty>"
  | _ -> List.iter
           (fun clause -> [clause] |> CNF.to_sentence |> Sentence.visualize |> print_endline)
           clauses


let print_assignment: Truth.assignment -> unit =
  fun assignment ->
  Hashtbl.iter
    (fun l v -> let s = (Letter.to_string l) ^ " " ^ (string_of_bool v) in
                print_endline s)
    assignment

