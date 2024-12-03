open Base

let empty_map = Map.empty (module String)
let add key value map = Map.set ~key ~data:value map

let sue =
  empty_map
  |> add "children" 3
  |> add "cats" 7
  |> add "samoyeds" 2
  |> add "pomeranians" 3
  |> add "akitas" 0
  |> add "vizslas" 0
  |> add "goldfish" 5
  |> add "trees" 3
  |> add "cars" 2
  |> add "perfumes" 1
;;

let to_sue i line =
  Pcre2.extract_all ~full_match:false ~pat:"([a-z]+): (\\d+)" line
  |> Array.fold ~init:empty_map ~f:(fun acc arr ->
    acc
    |> Map.set ~key:arr.(0) ~data:(Int.of_string arr.(1))
    |> Map.set ~key:"id" ~data:(i + 1))
;;

let one_of v lst =
  lst |> List.filter ~f:(fun l -> String.equal l v) |> List.is_empty |> not
;;

let get_id sue = Map.find_exn sue "id"

let sue_matches retroencabulator s =
  List.for_all
    ~f:(fun prop ->
      match Map.find sue prop with
      | Some x ->
        (match Map.find s prop with
         | Some y when retroencabulator && one_of prop [ "cats"; "trees" ] -> x < y
         | Some y when retroencabulator && one_of prop [ "pomeranians"; "goldfish" ] -> x > y
         | Some y -> x = y
         | None -> true)
      | None -> false)
    (Map.keys sue)
;;

let () =
  let sues = Tools.read_lines () |> List.mapi ~f:to_sue in
  let sue1 = sues |> List.filter ~f:(sue_matches false) |> List.hd_exn in
  let sue2 = sues |> List.filter ~f:(sue_matches true) |> List.hd_exn in
  Stdlib.Printf.printf "Solution 1: %d\n" (get_id sue1);
  Stdlib.Printf.printf "Solution 2: %d\n" (get_id sue2)
;;
