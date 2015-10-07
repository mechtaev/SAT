module Id :
  sig
    type t
    val compare: t -> t -> int
    val equal: t -> t -> bool
    val hash: t -> t -> int
    val mk_id: unit -> t
    val to_int: t -> int
  end

type t =
    Letter of Id.t
  | Conjunction of t * t
  | Disjunction of t * t
  | Negation of t

module Constructors :
  sig
    val mk_letter : unit -> t
    val ( &. ) : t -> t -> t
    val ( |. ) : t -> t -> t
    val ( ~. ) : t -> t
  end

val fold : (t -> 'a list -> 'a) -> t -> 'a

val to_conjuncts: t -> t list

val to_disjuncts: t -> t list

val of_conjuncts: t list -> t

val of_disjuncts: t list -> t

val visualize: t -> string list
