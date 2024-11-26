open Tools
module CharSet = Set.Make (Char)

let vovels = CharSet.of_list [ 'a'; 'e'; 'i'; 'o'; 'u' ]

let three_vovels word =
  let is_vovel c = CharSet.mem c vovels in
  let vovels = String.fold_left (fun a c -> if is_vovel c then a + 1 else a) 0 word in
  vovels >= 3
;;

let twice word = Pcre2.pmatch ~rex:(Pcre2.regexp "(.)\\1") word
let without_strings word = not (Pcre2.pmatch ~rex:(Pcre2.regexp "ab|cd|pq|xy") word)
let two_pairs word = Pcre2.pmatch ~rex:(Pcre2.regexp "(..).*\\1") word
let letter_between word = Pcre2.pmatch ~rex:(Pcre2.regexp "(.).\\1") word

let get_nice_words words preds =
  List.filter (fun word -> List.for_all (fun p -> p word) preds) words
;;

let () =
  let words = read_lines () in
  let nice1 = get_nice_words words [ three_vovels; twice; without_strings ] in
  let nice2 = get_nice_words words [ two_pairs; letter_between ] in
  Printf.printf "Solution 1: %d\n" (List.length nice1);
  Printf.printf "Solution 2: %d\n" (List.length nice2)
;;
