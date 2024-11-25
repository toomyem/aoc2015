open Tools

type box =
  { w : int
  ; h : int
  ; l : int
  }

let make_box line : box = Scanf.sscanf line "%dx%dx%d" (fun w h l -> { w; h; l })

let calc_area acc line =
  let b = make_box line in
  let b1 = b.w * b.h in
  let b2 = b.w * b.l in
  let b3 = b.h * b.l in
  let area = (2 * b1) + (2 * b2) + (2 * b3) in
  let extra = min (min b1 b2) b3 in
  acc + area + extra
;;

let calc_ribbon acc line =
  let b = make_box line in
  let b1 = (2 * b.w) + (2 * b.h) in
  let b2 = (2 * b.w) + (2 * b.l) in
  let b3 = (2 * b.h) + (2 * b.l) in
  let s = List.sort (fun a b -> a - b) [ b1; b2; b3 ] in
  let p = List.hd s in
  let extra = b.w * b.h * b.l in
  acc + p + extra
;;

let () =
  let lines = read_lines () in
  let total1 = List.fold_left calc_area 0 lines in
  let total2 = List.fold_left calc_ribbon 0 lines in
  Printf.printf "Solution 1: %d\n" total1;
  Printf.printf "Solution 2: %d\n" total2
;;
