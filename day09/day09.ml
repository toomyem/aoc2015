open Tools
open Base
open Graph

module G = struct
  module Edge = struct
    type t = int

    let compare = compare
    let default = 0
  end

  include Imperative.Graph.AbstractLabeled (String) (Edge)
end

let create_v h a =
  match Hashtbl.find h a with
  | Some v -> v
  | None ->
    let v = G.V.create a in
    Hashtbl.add_exn h ~key:a ~data:v;
    v
;;

let add_edge h g line =
  match Pcre2.extract ~full_match:false ~pat:"(\\S+) to (\\S+) = (\\S+)" line with
  | [| a; b; l |] ->
    let v1 = create_v h a in
    let v2 = create_v h b in
    G.E.create v1 (Int.of_string l) v2 |> G.add_edge_e g
  | _ -> failwith "Invalid syntax"
;;

let printf = Stdlib.Printf.printf

let () =
  let h = Hashtbl.create (module String) in
  let g = G.create () in
  read_lines () |> List.iter ~f:(add_edge h g);
  G.Mark.set (Hashtbl.find_exn h "Tristram") 1;
  printf "Day: 9, %d, %d\n" (G.nb_vertex g) (G.nb_edges g);
  G.iter_vertex (fun v -> printf "%d," (G.Mark.get v)) g
;;
