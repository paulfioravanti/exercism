#!/usr/bin/env bash

declare -r DNA="CGAT"
declare -r RNA="GCUA"

main () {
  local -r dna=$1
  check_dna
  echo "$dna" | tr $DNA $RNA
}

check_dna () {
  if [[ $dna =~ [^$DNA] ]]; then
    echo "Invalid nucleotide detected."
    exit 1
  fi
}

main "$@"
