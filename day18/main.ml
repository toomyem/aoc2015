open Base

let get board r c =
  let w = String.length (List.hd_exn board)
  and h = List.length board in
  if r >= 0 && r < h && c >= 0 && c < w
  then (
    let row = List.nth_exn board r in
    String.get row c)
  else '.'
;;

let count_on board =
  board
  |> List.map ~f:(fun row -> String.count ~f:(fun x -> Char.equal x '#') row)
  |> Tools.sum_of_ints
;;

let skip_0_0 pairs = List.filter ~f:(fun (x, y) -> x <> 0 || y <> 0) pairs

let count_neighbours board r c =
  List.cartesian_product [ -1; 0; 1 ] [ -1; 0; 1 ]
  |> skip_0_0
  |> List.map ~f:(fun (dr, dc) ->
    let nr = r + dr
    and nc = c + dc in
    get board nr nc)
  |> List.map ~f:(fun ch -> if Char.equal ch '#' then 1 else 0)
  |> Tools.sum_of_ints
;;

let calc_new_state board r c broken =
  let w = String.length (List.hd_exn board)
  and h = List.length board in
  if broken
     && ((r = 0 && c = 0)
         || (r = 0 && c = w - 1)
         || (c = 0 && r = h - 1)
         || (c = w - 1 && r = h - 1))
  then '#'
  else (
    match get board r c, count_neighbours board r c with
    | '#', n when n = 2 || n = 3 -> '#'
    | '.', n when n = 3 -> '#'
    | _ -> '.')
;;

let make_step board broken =
  List.mapi
    ~f:(fun r row -> String.mapi ~f:(fun c _ -> calc_new_state board r c broken) row)
    board
;;

let () =
  let rows = Tools.read_lines () in
  let b1 = ref rows in
  let b2 = ref rows in
  for _ = 1 to 100 do
    b1 := make_step !b1 false;
    b2 := make_step !b2 true
  done;
  let n1 = count_on !b1 in
  let n2 = count_on !b2 in
  Stdlib.Printf.printf "Solution 1: %d\n" n1;
  Stdlib.Printf.printf "Solution 2: %d\n" n2
;;
