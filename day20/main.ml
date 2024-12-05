open Base

let make_arr times max_houses =
  let size = 1_000_000 in
  let arr = Array.create ~len:size 0 in
  for n = 1 to size do
    let i = ref n in
    let houses = ref 0 in
    while !i <= size && !houses < max_houses do
      arr.(!i - 1) <- arr.(!i - 1) + (n * times);
      i := !i + n;
      houses := !houses + 1
    done
  done;
  arr
;;

let () =
  let limit = Tools.read_line () |> Int.of_string in
  let n1, _ = make_arr 10 1000000 |> Array.findi_exn ~f:(fun _ x -> x >= limit) in
  let n2, _ = make_arr 11 50 |> Array.findi_exn ~f:(fun _ x -> x >= limit) in
  Stdlib.Printf.printf "Solution 1: %d\n" (n1 + 1);
  Stdlib.Printf.printf "Solution 2: %d\n" (n2 + 1)
;;
