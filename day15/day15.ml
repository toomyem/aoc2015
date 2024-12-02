open Base

let rec distribute (n : int) (k : int) (current : int list) =
  if k = 1
  then [ current @ [ n ] ]
  else (
    let rec dist i acc =
      if i > n
      then acc
      else dist (i + 1) (acc @ distribute (n - i) (k - 1) (current @ [ i ]))
    in
    dist 0 [])
;;

type prop =
  { capacity : int
  ; durability : int
  ; flavor : int
  ; texture : int
  ; calories : int
  }

type cookie =
  { score : int
  ; calories : int
  }

let to_prop line =
  let arr =
    Pcre2.extract
      ~full_match:false
      ~pat:
        "capacity (-?\\d+), durability (-?\\d+), flavor (-?\\d+), texture (-?\\d+), \
         calories (-?\\d+)"
      line
    |> Array.map ~f:(fun m -> Int.of_string m)
  in
  { capacity = arr.(0)
  ; durability = arr.(1)
  ; flavor = arr.(2)
  ; texture = arr.(3)
  ; calories = arr.(4)
  }
;;

let get_capacity (p : prop) = p.capacity
let get_durability (p : prop) = p.durability
let get_flavour (p : prop) = p.flavor
let get_texture (p : prop) = p.texture
let get_calories (p : prop) = p.calories

let calc_prop (props : prop list) dist getter =
  List.fold2_exn ~init:0 ~f:(fun acc p d -> acc + (getter p * d)) props dist
;;

let calc (props : prop list) (dist : int list) : prop =
  { capacity = calc_prop props dist get_capacity
  ; durability = calc_prop props dist get_durability
  ; flavor = calc_prop props dist get_flavour
  ; texture = calc_prop props dist get_texture
  ; calories = calc_prop props dist get_calories
  }
;;

let up n = if n < 0 then 0 else n

let max_score cookies =
  List.max_elt ~compare:(fun a b -> a.score - b.score) cookies
  |> Option.value_exn
  |> fun c -> c.score
;;

let () =
  let props = Tools.read_lines () |> List.map ~f:to_prop in
  let dists = distribute 100 (List.length props) [] in
  let cookies =
    List.map ~f:(calc props) dists
    |> List.map ~f:(fun prop ->
      { score = up prop.capacity * up prop.durability * up prop.flavor * up prop.texture
      ; calories = up prop.calories
      })
  in
  let s1 = cookies |> max_score in
  let s2 = cookies |> List.filter ~f:(fun c -> c.calories = 500) |> max_score in
  Stdlib.Printf.printf "Solution 1: %d\n" s1;
  Stdlib.Printf.printf "Solution 2: %d\n" s2
;;
