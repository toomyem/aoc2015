open Tools

let valid md5 prefix = String.starts_with ~prefix md5

let find secret prefix =
  let rec find_n n =
    let md5 = secret ^ string_of_int n |> Digest.MD5.string |> Digest.MD5.to_hex in
    if valid md5 prefix then n else find_n (n + 1)
  in
  find_n 0
;;

let () =
  let secret = read_line () in
  let n1 = find secret "00000" in
  let n2 = find secret "000000" in
  Printf.printf "Solution 1: %d\n" n1;
  Printf.printf "Solution 2: %d\n" n2
;;
