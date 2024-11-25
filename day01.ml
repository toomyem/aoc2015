open Tools

let delta x =
  match x with
  | '(' -> 1
  | ')' -> -1
  | _ -> 0
;;

type t =
  { floor : int
  ; steps : int
  ; first : int
  }

let t0 = { floor = 0; steps = 0; first = 0 }

let update a c =
  let new_floor = a.floor + delta c in
  let new_steps = a.steps + 1 in
  let new_first = if a.first == 0 && new_floor == -1 then new_steps else a.first in
  { floor = new_floor; steps = new_steps; first = new_first }
;;

let () =
  let line = input_line stdin in
  let solution = String.fold_left update t0 line in
  print_endline ("Solution 1: " ^ string_of_int solution.floor);
  print_endline ("Solution 2: " ^ string_of_int solution.first)
;;
