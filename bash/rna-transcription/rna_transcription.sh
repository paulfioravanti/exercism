#!/usr/bin/env bash

declare -A RNA_TRANSCRIPTIONS=(
  ["C"]="G"
  ["G"]="C"
  ["A"]="U"
  ["T"]="A"
)

main () {
  if [[ -z "$1" ]]; then
    exit 0
  fi

  readonly dna=$(echo "$1" | grep -o .)
  local nucleotides=""

  for nucleotide in $dna; do
    local rna="${RNA_TRANSCRIPTIONS["$nucleotide"]}"
    if [[ -z $rna ]]; then
      echo "Invalid nucleotide detected."
      exit 1
    fi
    nucleotides+="$rna"
  done

  echo "$nucleotides"
}

main "$@"
