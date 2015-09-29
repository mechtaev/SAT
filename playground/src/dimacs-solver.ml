let terms =
  let open Term in
  let x = mk_var () in
  let y = mk_var () in
  let z = mk_var () in
  [And (Or (x, y), And (Neg x, z)); Neg (And (x, z))]

let _ = terms |> Term.visualize |> (List.iter print_endline)
