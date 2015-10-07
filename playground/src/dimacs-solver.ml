let _ =
  let file = Sys.argv.(1) in
  let cnf = Dimacs.load file in
  cnf |> CNF.to_form |> Form.visualize |> (List.iter print_endline)
