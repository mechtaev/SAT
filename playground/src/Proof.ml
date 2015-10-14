type t = CNF.Clause.t list


type checker = CNF.t -> t -> bool
