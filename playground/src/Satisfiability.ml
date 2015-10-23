type t = Satisfiable of Truth.assignment option
       | Unsatisfiable of Proof.t option
       | Unknown


type solver = CNF.t -> t
