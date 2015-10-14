open LibExt


type t = int


let last_letter = ref 0


let mk_letter () =
  let l = !last_letter in
  last_letter := l + 1;
  l


let to_int l = l


let compare = Pervasives.compare


let equal = (=)


let hash i = Hashtbl.hash


let to_string l =
  let rec to_char_list i =
    let to_char i = Char.chr ((Char.code 'a') + i) in
    let base = (Char.code 'z') - (Char.code 'a') + 1 in
    if i < base then
      [to_char i]
    else
      (to_char_list (i / base)) @ [to_char (i mod base)]
  in
  l |> to_char_list |> String.of_list

