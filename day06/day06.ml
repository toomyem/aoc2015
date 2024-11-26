open Tools
open Base

let printf = Stdlib.Printf.printf
let empty = Set.empty (module String)

type box =
  { x1 : int
  ; y1 : int
  ; x2 : int
  ; y2 : int
  }

let make_box x1 y1 x2 y2 =
  { x1 = Int.of_string x1
  ; y1 = Int.of_string y1
  ; x2 = Int.of_string x2
  ; y2 = Int.of_string y2
  }
;;

let fold box init f =
  let x = Sequence.range ~stop:`inclusive box.x1 box.x2 in
  let y = Sequence.range ~stop:`inclusive box.y1 box.y2 in
  let pairs = Sequence.cartesian_product x y in
  Sequence.fold pairs ~init ~f
;;

let set_toggle s elem = if Set.mem s elem then Set.remove s elem else Set.add s elem
let string_of_pair p = Printf.sprintf "%d,%d" (fst p) (snd p)
let turn_on acc box = fold box acc (fun s p -> Set.add s (string_of_pair p))
let turn_off acc box = fold box acc (fun s p -> Set.remove s (string_of_pair p))
let toggle acc box = fold box acc (fun s p -> set_toggle s (string_of_pair p))

let f acc w =
  let rex = Pcre2.regexp "(.+) (\\d+),(\\d+) through (\\d+),(\\d+)" in
  match Pcre2.extract ~rex ~full_match:false w with
  | [| "turn on"; x1; y1; x2; y2 |] -> turn_on acc (make_box x1 y1 x2 y2)
  | [| "turn off"; x1; y1; x2; y2 |] -> turn_off acc (make_box x1 y1 x2 y2)
  | [| "toggle"; x1; y1; x2; y2 |] -> toggle acc (make_box x1 y1 x2 y2)
  | _ -> acc
;;

let () =
  let lines = read_lines () in
  let lights = List.fold_left lines ~init:empty ~f in
  printf "Solution 1: %d\n" (Set.length lights)
;;
