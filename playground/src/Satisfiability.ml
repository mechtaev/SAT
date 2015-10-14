type t = Satisfiable of Interpretation.assignment option
       | Unsatisfiable of Proof.t option
       | Unknown


type solver = CNF.t -> t
