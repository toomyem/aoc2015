#!/bin/bash

set -euo pipefail

if [[ "$#" -eq 0 ]]
then
  echo "Usage: $0 <day>"
  exit
fi

day="$1"
day_padded=$(printf "day%02d" "$day")
input="${day_padded}/${day_padded}.input"
session="${SESSION:?is not set}"

[[ -f "$input" ]] || wget -O "$input" --header "Cookie: session=$session" "https://adventofcode.com/2015/day/$day/input"

if [[ "$#" -gt 1 ]]
then
  input="$2"
fi

dune build
if [[ "$input" = "-" ]]
then
"_build/default/${day_padded}/${day_padded}.exe"
else
"_build/default/${day_padded}/${day_padded}.exe" < "$input"
fi
