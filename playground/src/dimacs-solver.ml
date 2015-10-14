module F = Form.Constructors


let print_assignment: Interpretation.assignment -> unit =
  fun assignment ->
  Hashtbl.iter
    (fun l v -> let s = (Letter.to_string l) ^ " " ^ (string_of_bool v) in
                print_endline s)
    assignment


let _ =
  let file = Sys.argv.(1) in
  let cnf = Dimacs.load file in
  let _ = Utils.print_clauses cnf in
  let solver = DavisPutnam.solve in
  let open Satisfiability in
  match (solver cnf) with
  | Satisfiable (Some a)   -> print_endline "SAT"; print_assignment a
  | Satisfiable None       -> print_endline "SAT"
  | Unsatisfiable (Some p) -> print_endline "UNSAT"; Utils.print_clauses p
  | Unsatisfiable None     -> print_endline "UNSAT"
  | Unknown                -> print_endline "Unknown"
     
