open Tools
open Base

type phase =
  | Fly of int
  | Rest of int

type reindeer =
  { spd : int
  ; fly_time : int
  ; rest_time : int
  ; phase : phase
  ; points : int
  ; dist : int
  }

let to_reindeer line =
  let arr =
    Pcre2.extract ~full_match:false ~pat:"(\\w+).*?(\\d+).*?(\\d+).*?(\\d+)" line
  in
  { spd = Int.of_string arr.(1)
  ; fly_time = Int.of_string arr.(2)
  ; rest_time = Int.of_string arr.(3)
  ; phase = Rest 0
  ; points = 0
  ; dist = 0
  }
;;

let move reindeer : reindeer =
  match reindeer.phase with
  | Fly 0 -> { reindeer with phase = Rest (reindeer.rest_time - 1) }
  | Fly n -> { reindeer with phase = Fly (n - 1); dist = reindeer.dist + reindeer.spd }
  | Rest 0 ->
    { reindeer with
      phase = Fly (reindeer.fly_time - 1)
    ; dist = reindeer.dist + reindeer.spd
    }
  | Rest n -> { reindeer with phase = Rest (n - 1) }
;;

let get_dist r = r |> Option.value_exn |> fun r -> r.dist
let get_points r = r |> Option.value_exn |> fun r -> r.points

let calc_points reindeers =
  let max_dist =
    List.max_elt ~compare:(fun a b -> a.dist - b.dist) reindeers |> get_dist
  in
  List.map
    ~f:(fun r -> if r.dist = max_dist then { r with points = r.points + 1 } else r)
    reindeers
;;

let move_all reindeers =
  let rec calc0 n reindeers =
    if n = 0
    then reindeers
    else calc0 (n - 1) (reindeers |> List.map ~f:move |> calc_points)
  in
  calc0 2503 reindeers
;;

let () =
  let reindeers = read_lines () |> List.map ~f:to_reindeer |> move_all in
  let dist1 =
    reindeers |> List.max_elt ~compare:(fun a b -> a.dist - b.dist) |> get_dist
  in
  let dist2 =
    reindeers |> List.max_elt ~compare:(fun a b -> a.points - b.points) |> get_points
  in
  Stdlib.Printf.printf "Solution 1: %d\n" dist1;
  Stdlib.Printf.printf "Solution 2: %d\n" dist2
;;
