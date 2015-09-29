type var_id

type t =
    Var of var_id
  | Neg of t
  | And of t * t
  | Or of t * t
  | Xor of t * t
  | Iff of t * t

val mk_var : unit -> t

val fold : (t -> 'a list -> 'a) -> t -> 'a

val visualize : t list -> string list
