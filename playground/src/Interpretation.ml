open LibExt

       
type assignment = (Letter.t, bool) Hashtbl.t


let of_form: assignment -> Form.t -> bool =
  fun assignment form ->
  let eval = function
    | Form.Letter l      -> (fun _ -> Hashtbl.find assignment l)
    | Form.Conjunction _ -> (function [x; y] -> x && y)
    | Form.Disjunction _ -> (function [x; y] -> x || y)
    | Form.Negation _    -> (function [x]    -> not x)
  in
  Form.fold eval form


let of_cnf: assignment -> CNF.t -> bool =
  fun assignment cnf ->
  let eval_clause clause =
    CNF.Clause.fold
      CNF.Literal.(function Letter l -> (fun v -> v || Hashtbl.find assignment l)
                          | Negation l -> (fun v -> v || (not @@ Hashtbl.find assignment l)))
      clause
      false
  in
  List.all @@ (List.map eval_clause) @@ cnf
                                    
