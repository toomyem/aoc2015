(lang dune 3.16)

(name aoc2015)

(generate_opam_files true)

(source
 (github toomyem/aoc2015))

(authors "toomyem")

(license MIT)

(documentation https://github.com/toomyem/aoc2015)

(package
 (name aoc2015)
 (synopsis "AOC 2015")
 (description "Solutions for Advent of Code 2015")
 (depends ocaml dune pcre2)
 (allow_empty)
)

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
