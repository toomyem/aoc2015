let change line =
  Pcre2.substitute
    ~pat:"(.)\\1*"
    ~subst:(fun s ->
      let n = String.length s in
      Printf.sprintf "%d%c" n s.[0])
    line
;;

let rec convert n line =
  match n with
  | 0 -> line
  | _ -> convert (n - 1) (change line)
;;

let () =
  let line = Tools.read_line () in
  let converted1 = convert 40 line in
  let converted2 = convert 50 line in
  Stdlib.Printf.printf "Solution 1: %d\n" (String.length converted1);
  Stdlib.Printf.printf "Solution 2: %d\n" (String.length converted2)
;;
