#!/usr/bin/env bash

declare -ri NUMBER_OF_LETTERS_IN_ALPHABET=26

main () {
  local -r sentence=$1

  if [[ $(letter_count) -eq $NUMBER_OF_LETTERS_IN_ALPHABET ]]; then
    echo true
  else
    echo false
  fi
}

letter_count () {
  echo "$sentence" |
  tr "[:upper:]" "[:lower:]" | # lowercase the string
  tr -cd "[:alpha:]" |         # remove non-alpha characters
  grep --only-matching . |     # put each character on a new line
  sort --unique |              # get unique lines
  wc -l                        # get lines word count
}

main "$@"
