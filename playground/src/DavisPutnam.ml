open LibExt


let blatant_contradiction cnf =
  let letters = CNF.letters cnf in
  let unit_clauses = List.filter CNF.is_unit cnf in
  List.fold_left
    (fun acc l -> acc || ((CNF.mem (Letter l) unit_clauses) && (CNF.mem (Negation l) unit_clauses)))
    false
    letters


let tautology c =
  let letters = CNF.letters [c] in
  List.fold_left
    (fun acc l -> acc || ((CNF.Clause.mem (Letter l) c) && (CNF.Clause.mem (Negation l) c)))
    false
    letters


let letter_in_clause l c = ((CNF.Clause.mem (Letter l) c) || (CNF.Clause.mem (Negation l) c))


let solve: Satisfiability.solver =
  fun cnf ->
  let different a b = not (CNF.Clause.equal a b) in
  let all_different a l = List.map (different a) l |> List.all in
  let letters = CNF.letters cnf in
  let open CNF.Literal in
  let proof = ref [] in
  let res = ref cnf in
  let found_contradiction = ref false in
  let letters_for_resolution = ref letters in
  while (not !found_contradiction) && (List.length !letters_for_resolution) > 0 do
    res := List.filter (fun c -> not (tautology c)) !res;
    if blatant_contradiction !res then
      found_contradiction := true
    else begin
        let l = List.hd !letters_for_resolution in
        letters_for_resolution := List.tl !letters_for_resolution;
        let unrelated = List.filter (fun c -> not (letter_in_clause l c)) !res in
        let resolvent = Resolution.resolve_all l !res in
        proof := !proof @ resolvent;
        let unique =
          List.fold_left
            (fun clauses next -> if all_different next clauses then next :: clauses else clauses)
            (List.rev unrelated)
            resolvent
        in
        res := List.rev unique
      end
  done;
  if blatant_contradiction !res then
    Satisfiability.Unsatisfiable (Some !proof)
  else
    let assignment: Truth.assignment = Hashtbl.create 1024 in
    let literals = List.fold_left CNF.Clause.union CNF.Clause.empty !res |> CNF.Clause.elements in
    List.iter (function Letter l -> Hashtbl.add assignment l true
                      | Negation l -> Hashtbl.add assignment l false)
              literals;
    Satisfiability.Satisfiable (Some assignment)
