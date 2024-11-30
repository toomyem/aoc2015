open Tools
open Base

type data =
  { spd : int
  ; fly_time : int
  ; rest_time : int
  }

let to_data line =
  let arr =
    Pcre2.extract ~full_match:false ~pat:"(\\w+).*?(\\d+).*?(\\d+).*?(\\d+)" line
  in
  { spd = Int.of_string arr.(1)
  ; fly_time = Int.of_string arr.(2)
  ; rest_time = Int.of_string arr.(3)
  }
;;

let calc_dist data =
  let total = 2503 in
  let n = total / (data.fly_time + data.rest_time) in
  let r = total - (n * (data.fly_time + data.rest_time)) in
  (n * data.spd * data.fly_time)
  + if r >= data.fly_time then data.spd * data.fly_time else data.spd * r
;;

let () =
  let dist =
    read_lines ()
    |> List.map ~f:to_data
    |> List.map ~f:calc_dist
    |> List.max_elt ~compare:Int.compare
    |> Option.value_exn
  in
  Stdlib.Printf.printf "Solution 1: %d\n" dist
;;
