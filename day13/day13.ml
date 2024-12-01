open Base

let empty = Map.empty (module String)
let make_key name1 name2 = name1 ^ "-" ^ name2
let sum l = List.fold ~init:0 ~f:(fun acc x -> acc + x) l

let to_person acc line =
  let map, set = acc in
  let arr =
    Pcre2.extract ~full_match:false ~pat:"(\\w+) would (gain|lose) (\\d+).* (\\w+)" line
  in
  let name = arr.(0) in
  let pref = Int.of_string arr.(2) * if String.equal arr.(1) "gain" then 1 else -1 in
  let key = make_key name arr.(3) in
  Map.set map ~key ~data:pref, Set.add set name
;;

let get_neighbours names i =
  let len = List.length names in
  let next = (i + 1) % len in
  let prev = (i + len - 1) % len in
  [| List.nth_exn names prev; List.nth_exn names next |]
;;

let calc_pref map names =
  List.mapi names ~f:(fun i n ->
    let neighbours = get_neighbours names i in
    Map.find_exn map (make_key n neighbours.(0))
    + Map.find_exn map (make_key n neighbours.(1)))
  |> sum
;;

let calc_max map names =
  names
  |> Set.to_list
  |> Tools.permute
  |> List.map ~f:(calc_pref map)
  |> List.max_elt ~compare:Int.compare
  |> Option.value_exn
;;

let myself = "myself"

let add_myself map names =
  Set.fold
    ~init:map
    ~f:(fun m n ->
      m
      |> Map.set ~key:(make_key myself n) ~data:0
      |> Map.set ~key:(make_key n myself) ~data:0)
    names
;;

let () =
  let map, names =
    Tools.read_lines () |> List.fold ~init:(empty, Set.empty (module String)) ~f:to_person
  in
  let max1 = calc_max map names in
  let max2 = calc_max (add_myself map names) (Set.add names myself) in
  Stdlib.Printf.printf "Solution 1: %d\n" max1;
  Stdlib.Printf.printf "Solution 2: %d\n" max2
;;
