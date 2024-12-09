open Base

let fill bottles n =
  let h = Hashtbl.create (module Int) in
  let rec fill0 bottles n x : unit =
    if n = 0
    then Hashtbl.incr h (List.length x)
    else (
      match bottles with
      | b :: rest ->
        if n < b
        then fill0 rest n x
        else (
          fill0 rest n x;
          fill0 rest (n - b) (b :: x))
      | [] -> ())
  in
  fill0 bottles n [];
  h
;;

let () =
  let bottles = Tools.read_lines () |> List.map ~f:Int.of_string in
  let h = fill bottles 150 in
  let n1 = Hashtbl.fold ~init:0 ~f:(fun ~key:_ ~data:v acc -> acc + v) h in
  let n2 = Hashtbl.keys h |> List.min_elt ~compare:Int.compare |> Option.value_exn |> Hashtbl.find_exn h in
  Stdlib.Printf.printf "Solution 1: %d\n" n1;
  Stdlib.Printf.printf "Solution 2: %d\n" n2
;;
