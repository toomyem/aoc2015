open Base
open Stdio

let read_line () = In_channel.input_line stdin |> Option.value_exn

let read_lines () =
  let rec read_lines0 acc =
    try
      let line = read_line () in
      line :: read_lines0 acc
    with
    | _ -> acc
  in
  read_lines0 []
;;

let rec permute (l : 'a list) : 'a list list =
  let insert a l =
    let len = List.length l in
    List.range ~stop:`inclusive 0 len
    |> List.map ~f:(fun i ->
      List.concat [ List.sub l ~pos:0 ~len:i; [ a ]; List.sub l ~pos:i ~len:(len - i) ])
  in
  match l with
  | [] | [ _ ] -> [ l ]
  | hd :: tl ->
    let p = permute tl in
    if List.is_empty p then [ [ hd ] ] else p |> List.map ~f:(insert hd) |> List.concat
;;
