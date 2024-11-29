open Base

let rec contains_red (v : (string * Yojson.Basic.t) list) : bool =
  match v with
  | [] -> false
  | x :: l ->
    (match snd x with
     | `String "red" -> true
     | _ -> contains_red l)
;;

let sum l = List.fold_left ~init:0 ~f:(fun acc x -> acc + x) l

let rec calc_sum no_red (json : Yojson.Basic.t) =
  match json with
  | `Int x -> x
  | `String _ | `Null | `Float _ | `Bool _ -> 0
  | `List v -> v |> List.map ~f:(calc_sum no_red) |> sum
  | `Assoc v ->
    if no_red && contains_red v
    then 0
    else v |> List.map ~f:snd |> List.map ~f:(calc_sum no_red) |> sum
;;

let () =
  let data = Tools.read_line () in
  let json = Yojson.Basic.from_string data in
  Stdlib.Printf.printf "Solution 1: %d\n" (calc_sum false json);
  Stdlib.Printf.printf "Solution 2: %d\n" (calc_sum true json)
;;
