#!/usr/bin/env bash

DNA="CGAT"
RNA="GCUA"
DNA_STRAND="^[$DNA]*$"

main () {
  if [[ $1 =~ $DNA_STRAND ]]; then
    echo "$1" | tr "$DNA" "$RNA"
  else
    echo "Invalid nucleotide detected."
    exit 1
  fi
}

main "$@"
