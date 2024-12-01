open Base

type str_set = (String.t, String.comparator_witness) Set.t
type str_map = (String.t, int, String.comparator_witness) Map.t

type data =
  { names : str_set
  ; graph : str_map
  }

let empty_data = { names = Set.empty (module String); graph = Map.empty (module String) }
let make_key n1 n2 = n1 ^ "->" ^ n2

let to_data acc line =
  let arr = Pcre2.extract ~full_match:false ~pat:"(\\w+) to (\\w+) = (\\d+)" line in
  let name_from = arr.(0)
  and name_to = arr.(1)
  and dist = Int.of_string arr.(2) in
  let new_names = Set.add (Set.add acc.names name_from) name_to in
  { names = new_names
  ; graph =
      acc.graph
      |> Map.set ~key:(make_key name_from name_to) ~data:dist
      |> Map.set ~key:(make_key name_to name_from) ~data:dist
  }
;;

let calc_dist (graph : str_map) names =
  List.fold
    ~init:(List.hd_exn names, 0)
    ~f:(fun (prev, sum) name -> name, sum + Map.find_exn graph (make_key prev name))
    (List.drop names 1)
  |> snd
;;

let () =
  let data = Tools.read_lines () |> List.fold ~init:empty_data ~f:to_data in
  let p = data.names |> Set.to_list |> Tools.permute in
  let dists = List.map ~f:(calc_dist data.graph) p in
  let n1 = List.min_elt ~compare:Int.compare dists |> Option.value_exn in
  let n2 = List.max_elt ~compare:Int.compare dists |> Option.value_exn in
  Stdlib.Printf.printf "Solution 1: %d\n" n1;
  Stdlib.Printf.printf "Solution 2: %d\n" n2
;;
