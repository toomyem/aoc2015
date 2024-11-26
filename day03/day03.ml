open Tools

type position =
  { x : int
  ; y : int
  }

module StrSet = Set.Make (String)

type state =
  { pos : position
  ; visited : StrSet.t
  }

type who =
  | Santa
  | Robo

type state2 =
  { pos_santa : position
  ; pos_robo : position
  ; visited : StrSet.t
  ; turn : who
  }

let move pos c =
  match c with
  | '>' -> { pos with x = pos.x + 1 }
  | '<' -> { pos with x = pos.x - 1 }
  | '^' -> { pos with y = pos.y - 1 }
  | 'v' -> { pos with y = pos.y + 1 }
  | _ -> pos
;;

let pos0 = { x = 0; y = 0 }
let string_of_pos pos = Printf.sprintf "%dx%d" pos.x pos.y
let set0 = StrSet.add (string_of_pos pos0) StrSet.empty

let calc acc c =
  let new_pos = move acc.pos c in
  { pos = new_pos; visited = StrSet.add (string_of_pos new_pos) acc.visited }
;;

let calc2 acc c =
  let new_pos_santa = move acc.pos_santa c in
  let new_pos_robo = move acc.pos_robo c in
  match acc.turn with
  | Santa ->
    { pos_santa = new_pos_santa
    ; pos_robo = acc.pos_robo
    ; visited = StrSet.add (string_of_pos new_pos_santa) acc.visited
    ; turn = Robo
    }
  | Robo ->
    { pos_santa = acc.pos_santa
    ; pos_robo = new_pos_robo
    ; visited = StrSet.add (string_of_pos new_pos_robo) acc.visited
    ; turn = Santa
    }
;;

let () =
  let line = read_line () in
  let state = String.fold_left calc { pos = pos0; visited = set0 } line in
  let state2 =
    String.fold_left
      calc2
      { pos_santa = pos0; pos_robo = pos0; visited = set0; turn = Santa }
      line
  in
  Printf.printf "Solution 1: %d\n" (StrSet.cardinal state.visited);
  Printf.printf "Solution 2: %d\n" (StrSet.cardinal state2.visited)
;;
