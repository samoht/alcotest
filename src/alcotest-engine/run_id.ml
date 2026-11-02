type t = string

let length = 8
let alphabet = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ"
let random = lazy (Random.State.make_self_init ())

let v () =
  String.init length (fun _ ->
      alphabet.[Random.State.int (Lazy.force random) (String.length alphabet)])

let of_string s =
  let rec valid i =
    i = String.length s || (String.contains alphabet s.[i] && valid (i + 1))
  in
  if String.length s = length && valid 0 then Some s else None

let to_string t = t
