#!/usr/bin/env bash

declare -ri NUMBER_OF_LETTERS_IN_ALPHABET=26

main () {
  if pangram "$@"; then
    echo true
  else
    echo false
  fi
}

pangram () {
  [[ $(letter_count "$@") -eq $NUMBER_OF_LETTERS_IN_ALPHABET ]]
}

letter_count () {
  echo "$1" |
  tr "[:upper:]" "[:lower:]" | # lowercase the string
  tr -cd "[:alpha:]" |         # remove non-alpha characters
  grep --only-matching . |     # put each character on a new line
  sort --unique |              # get unique lines
  wc -l                        # get lines word count
}

main "$@"
