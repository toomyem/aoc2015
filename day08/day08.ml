open Tools
open Base

let decode s =
  let rex = Pcre2.regexp_or [ "\\\\\\\\"; "\\\\\""; "\\\\x.." ] in
  Pcre2.replace ~rex ~templ:"_" s
  |> String.chop_prefix_if_exists ~prefix:"\""
  |> String.chop_suffix_if_exists ~suffix:"\""
;;

let encode s =
  let rex = Pcre2.regexp_or [ "\""; "\\\\" ] in
  let n = Pcre2.replace ~rex ~templ:"__" s in
  "\"" ^ n ^ "\""
;;

let calc lines f =
  let n1 = lines |> List.fold_left ~f:(fun acc s -> acc + String.length s) ~init:0 in
  let n2 =
    lines |> List.map ~f |> List.fold_left ~f:(fun acc s -> acc + String.length s) ~init:0
  in
  abs (n1 - n2)
;;

let () =
  let lines = read_lines () in
  let n1 = calc lines decode
  and n2 = calc lines encode in
  Stdlib.Printf.printf "Solution 1: %d\n" n1;
  Stdlib.Printf.printf "Solution 2: %d\n" n2
;;
