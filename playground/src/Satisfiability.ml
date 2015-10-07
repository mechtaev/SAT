type assignment = (Id.t, bool) Hashtbl.t

type proof = unit

type t = Satiafiable of assignment option
       | Unsatisfiable of proof option
       | Unknown

type solver = CNF.t -> t
