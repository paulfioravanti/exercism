#!/usr/bin/env bash

DNA="CGAT"
RNA="GCUA"

main () {
  if [[ $1 =~ [^$DNA] ]]; then
    echo "Invalid nucleotide detected."
    exit 1
  fi
  echo "$1" | tr "$DNA" "$RNA"
}

main "$@"
