open Tools
open Base

module Pair = struct
  module T = struct
    type t = int * int

    let compare a b =
      match Int.compare (fst a) (fst b) with
      | 0 -> Int.compare (snd a) (snd b)
      | x -> x
    ;;

    let sexp_of_t x = Sexp.List [ Int.sexp_of_t (fst x); Int.sexp_of_t (snd x) ]
  end

  include T
  include Comparable.Make (T)
end

module Box = struct
  type t =
    { x1 : int
    ; y1 : int
    ; x2 : int
    ; y2 : int
    }

  let make x1 y1 x2 y2 =
    { x1 = Int.of_string x1
    ; y1 = Int.of_string y1
    ; x2 = Int.of_string x2
    ; y2 = Int.of_string y2
    }
  ;;
end

let empty = Map.empty (module Pair)
let turn_on lights pos = Map.set lights ~key:pos ~data:1
let turn_off lights pos = Map.remove lights pos

let toggle lights pos =
  Map.change lights pos ~f:(fun i ->
    match i with
    | Some _ -> None
    | None -> Some 1)
;;

let turn_on2 lights pos =
  Map.change lights pos ~f:(fun i ->
    match i with
    | Some x -> Some (x + 1)
    | None -> Some 1)
;;

let turn_off2 lights pos =
  Map.change lights pos ~f:(fun i ->
    match i with
    | Some x when x > 1 -> Some (x - 1)
    | Some _ | None -> None)
;;

let toggle2 lights pos =
  Map.change lights pos ~f:(fun i ->
    match i with
    | Some x -> Some (x + 2)
    | None -> Some 2)
;;

type cmd =
  | TurnOn of Box.t
  | TurnOff of Box.t
  | Toggle of Box.t

let to_cmd line =
  let rex = Pcre2.regexp "(.+) (\\d+),(\\d+) through (\\d+),(\\d+)" in
  match Pcre2.extract ~rex ~full_match:false line with
  | [| "turn on"; x1; y1; x2; y2 |] -> TurnOn (Box.make x1 y1 x2 y2)
  | [| "turn off"; x1; y1; x2; y2 |] -> TurnOff (Box.make x1 y1 x2 y2)
  | [| "toggle"; x1; y1; x2; y2 |] -> Toggle (Box.make x1 y1 x2 y2)
  | _ -> failwith "Invalid command"
;;

let to_pairs (box : Box.t) =
  Sequence.cartesian_product
    (Sequence.range ~stop:`inclusive box.x1 box.x2)
    (Sequence.range ~stop:`inclusive box.y1 box.y2)
;;

let process cmds ~init ~turn_on ~turn_off ~toggle =
  List.fold cmds ~init ~f:(fun lights cmd ->
    match cmd with
    | TurnOn box -> Sequence.fold (to_pairs box) ~init:lights ~f:turn_on
    | TurnOff box -> Sequence.fold (to_pairs box) ~init:lights ~f:turn_off
    | Toggle box -> Sequence.fold (to_pairs box) ~init:lights ~f:toggle)
;;

let printf = Stdlib.Printf.printf

let () =
  let cmds = read_lines () |> List.map ~f:to_cmd in
  let lights1 = process cmds ~init:empty ~turn_on ~turn_off ~toggle in
  let lights2 =
    process cmds ~init:empty ~turn_on:turn_on2 ~turn_off:turn_off2 ~toggle:toggle2
  in
  printf
    "Solution 1: %d\n"
    (Map.fold lights1 ~init:0 ~f:(fun ~key:_ ~data:v acc -> acc + v));
  printf
    "Solution 2: %d\n"
    (Map.fold lights2 ~init:0 ~f:(fun ~key:_ ~data:v acc -> acc + v))
;;
