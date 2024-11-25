#!/bin/bash

set -euo pipefail

if [[ "$#" -eq 0 ]]
then
  echo "Usage: $0 <day>"
  exit
fi

day="$1"
day_padded=$(printf "%02d" "$day")
input="day${day_padded}.input"
session="${SESSION:?is not set}"

[[ -f "$input" ]] || wget -O "$input" --header "Cookie: session=$session" "https://adventofcode.com/2015/day/$day/input"

if [[ "$#" -gt 1 ]]
then
  input="$2"
fi

ocamlc -c tools.mli
ocamlc -c tools.ml
ocamlc -c "day${day_padded}.ml"
ocamlc -o "day${day_padded}" tools.ml "day${day_padded}".ml
"./day${day_padded}" < "$input" && rm "day${day_padded}"{,.cmi,.cmo}
