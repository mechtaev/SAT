open LibExt

let load file =
  let ids: (int, Form.Id.t) Hashtbl.t = Hashtbl.create 1024 in
  let literal_of_int i =
    let id =
      if Hashtbl.mem ids (abs i) then
        Hashtbl.find ids (abs i)
      else
        let id = Form.Id.mk_id () in
        Hashtbl.add ids (abs i) id;
        id
    in
    if i > 0 then
      CNF.Literal.Letter id
    else
      CNF.Literal.Negation id
  in
  let clauses = ref [] in
  let nbvar = ref (-1) in
  let nbclauses = ref (-1) in
  let channel = open_in file in
  let finished = ref false in
  while not !finished do
    try
      let line = input_line channel in
      if (String.length line) = 0 || line.[0] = 'c' then
        ()
      else if line.[0] = 'p' then
        let _ :: _ :: nbvar_str :: nbclauses_str :: [] = String.split_whitespace line in
        nbvar := int_of_string nbvar_str;
        nbclauses := int_of_string nbclauses_str
      else
        let clause = line |> String.split_whitespace
                          |> (List.map int_of_string)
                          |> (List.filter (fun x -> x <> 0))
                          |> (List.map literal_of_int)
                          |> CNF.LiteralSet.of_list
        in
        clauses := clause :: !clauses
    with End_of_file ->
      close_in channel;
      finished := true
  done;
  List.rev !clauses

let dump: CNF.t -> string -> unit =
  fun cnf file -> not_implemented ()
