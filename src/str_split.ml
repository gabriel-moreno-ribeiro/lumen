(* Split a string on a separator substring (the standard library only splits on a char). *)

let split (s : string) (sep : string) : string list =
  let n = String.length s and m = String.length sep in
  let rec go start i acc =
    if i > n - m then List.rev (String.sub s start (n - start) :: acc)
    else if String.sub s i m = sep then go (i + m) (i + m) (String.sub s start (i - start) :: acc)
    else go start (i + 1) acc
  in
  if m = 0 then [ s ] else go 0 0 []
