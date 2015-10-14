open LibExt

(* Assume one clause has l, another ~l *)
let resolve: Letter.t -> CNF.Clause.t -> CNF.Clause.t -> CNF.Clause.t =
  fun l a b ->
  let open CNF.Literal in 
  let remove l c = CNF.Clause.remove (Letter l) @@ CNF.Clause.remove (Negation l) @@ c in
  CNF.Clause.union (remove l a) (remove l b)


(* Return resolvent with possible repetitions *)
let resolve_all: Letter.t -> CNF.t -> CNF.t =
  fun l cnf ->
  let open CNF.Literal in 
  let with_letter = List.filter (CNF.Clause.mem (Letter l)) cnf in
  let with_negation = List.filter (CNF.Clause.mem (Negation l)) cnf in
  List.cartesian with_letter with_negation |> List.map (fun (a, b) -> resolve l a b)
  
  
                                   
