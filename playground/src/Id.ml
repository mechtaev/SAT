open LibExt

type t = int

let last_id = ref 0

let mk_id () =
  let id = !last_id in
  last_id := id + 1;
  id

let to_int id = id

let compare = Pervasives.compare

let equal = (=)

let hash i = Hashtbl.hash

let to_string id =
  let rec to_char_list id =
    let to_char i = Char.chr ((Char.code 'a') + i) in
    let base = (Char.code 'z') - (Char.code 'a') + 1 in
    if id < base then
      [to_char id]
    else
      (to_char_list (id / base)) @ [to_char (id mod base)]
  in
  id |> to_char_list |> String.of_list

