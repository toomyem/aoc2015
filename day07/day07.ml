open Tools
open Base

let empty = Map.empty (module String)

type gate =
  | AND of string * string
  | OR of string * string
  | NOT of string
  | LSHIFT of string * string
  | RSHIFT of string * string
  | PASS of string

let to_cmd map line =
  match Pcre2.split ~pat:"\\s+" line with
  | [ a; "AND"; b; "->"; c ] -> Map.set map ~key:c ~data:(AND (a, b))
  | [ a; "OR"; b; "->"; c ] -> Map.set map ~key:c ~data:(OR (a, b))
  | [ a; "LSHIFT"; b; "->"; c ] -> Map.set map ~key:c ~data:(LSHIFT (a, b))
  | [ a; "RSHIFT"; b; "->"; c ] -> Map.set map ~key:c ~data:(RSHIFT (a, b))
  | [ "NOT"; a; "->"; c ] -> Map.set map ~key:c ~data:(NOT a)
  | [ a; "->"; c ] -> Map.set map ~key:c ~data:(PASS a)
  | _ -> failwith "Invalid command"
;;

let is_number v =
  try
    let _ = Int.of_string v in
    true
  with
  | Failure _ -> false
;;

let calc_gate map name =
  let h = Hashtbl.create (module String) in
  let rec calc name =
    match Hashtbl.find h name with
    | Some v -> v
    | None ->
      if is_number name
      then Int.of_string name
      else (
        let v =
          try
            match Map.find_exn map name with
            | AND (a, b) -> calc a land calc b
            | OR (a, b) -> calc a lor calc b
            | NOT a -> lnot (calc a)
            | LSHIFT (a, b) -> calc a lsl calc b
            | RSHIFT (a, b) -> calc a lsr calc b
            | PASS a -> calc a
          with
          | Not_found_s _ -> failwith ("Not found: " ^ name)
        in
        let _ = Hashtbl.add h ~key:name ~data:v in
        v)
  in
  calc name
;;

let modify map signal = Map.set map ~key:"b" ~data:(PASS (Int.to_string signal))

let () =
  let map = read_lines () |> List.fold ~f:to_cmd ~init:empty in
  let a1 = calc_gate map "a" in
  let a2 = calc_gate (modify map a1) "a" in
  Stdlib.Printf.printf "Solution 1: %d\n" a1;
  Stdlib.Printf.printf "Solution 2: %d\n" a2
;;
