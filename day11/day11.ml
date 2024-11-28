open Base

type state =
  { prev : char option
  ; count : int
  }

let rule1 str =
  String.fold_until
    ~init:{ prev = None; count = 0 }
    ~f:(fun acc ch ->
      match acc.prev with
      | None -> Continue { prev = Some ch; count = 1 }
      | Some x ->
        if Char.to_int x + 1 = Char.to_int ch
        then
          if acc.count = 2
          then Stop true
          else Continue { prev = Some ch; count = acc.count + 1 }
        else Continue { prev = Some ch; count = 1 })
    ~finish:(fun _ -> false)
    str
;;

let rule2 str =
  List.for_all [ 'i'; 'o'; 'l' ] ~f:(fun ch ->
    match String.index str ch with
    | None -> true
    | _ -> false)
;;

let rule3 str = Pcre2.pmatch ~pat:"(.)\\1.*(.)\\2" str

let replace_char i ch str =
  String.concat
    ~sep:""
    [ String.sub ~pos:0 ~len:i str
    ; Char.to_string ch
    ; String.sub ~pos:(i + 1) ~len:(String.length str - i - 1) str
    ]
;;

let inc_char ch = 1 + Char.to_int ch |> Char.of_int_exn

let increment str =
  let rec inc0 i data =
    if i < 0
    then data
    else (
      match data.[i] with
      | 'z' -> replace_char i 'a' data |> inc0 (i - 1)
      | x -> replace_char i (inc_char x) data)
  in
  inc0 (String.length str - 1) str
;;

let rec next_pass str =
  let next = increment str in
  if List.for_all [ rule1; rule2; rule3 ] ~f:(fun rule -> rule next)
  then next
  else next_pass next
;;

let () =
  let pass = Tools.read_line () in
  let next = next_pass pass in
  Stdlib.Printf.printf "Solution 1: %s\n" next;
  Stdlib.Printf.printf "Solution 2: %s\n" (next_pass next)
;;
