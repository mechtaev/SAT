module F = Sentence.Constructors

let _ =
  let file = Sys.argv.(1) in
  let cnf = Dimacs.load file in
  let _ = Utils.print_clauses cnf in
  let solver = DavisPutnam.solve in
  let open Satisfiability in
  match (solver cnf) with
  | Satisfiable (Some a)   -> print_endline "SAT"; Utils.print_assignment a
  | Satisfiable None       -> print_endline "SAT"
  | Unsatisfiable (Some p) -> print_endline "UNSAT"; Utils.print_clauses p
  | Unsatisfiable None     -> print_endline "UNSAT"
  | Unknown                -> print_endline "Unknown"
     
