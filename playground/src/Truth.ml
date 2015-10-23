open LibExt

       
type assignment = (Letter.t, bool) Hashtbl.t


let of_sentence: assignment -> Sentence.t -> bool =
  fun assignment sentence ->
  let eval = function
    | Sentence.Letter l      -> (fun _ -> Hashtbl.find assignment l)
    | Sentence.Conjunction _ -> (function [x; y] -> x && y)
    | Sentence.Disjunction _ -> (function [x; y] -> x || y)
    | Sentence.Negation _    -> (function [x]    -> not x)
  in
  Sentence.fold eval sentence


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
                                    
