let rec read_lines0 acc =
  try
    let line = read_line () in
    line :: read_lines0 acc
  with
  | End_of_file -> acc
;;

let read_line () = input_line stdin
let read_lines () = read_lines0 []