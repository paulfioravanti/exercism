#!/usr/bin/env bash

declare -r LETTERS="[A-Za-z]"

main () {
  local -r message=$(echo "$1" | tr -d "[:space:]")
  local -r shouting=$(is_shouting "$message")
  local -r question=$(is_question "$message")

  if [[ -z $message ]]; then
    echo "Fine. Be that way!"
  elif [[ $shouting -eq 0 && $question -eq 0 ]]; then
    echo "Calm down, I know what I'm doing!"
  elif [[ $shouting -eq 0 ]]; then
    echo "Whoa, chill out!"
  elif [[ $question -eq 0 ]]; then
    echo "Sure."
  else
    echo "Whatever."
  fi
}

is_shouting () {
  if [[ ! "$1" =~ $LETTERS ]]; then
    echo 1
  else
    local -r shout=$(echo "$1" | tr "[:lower:]" "[:upper:]")
    test "$1" = "$shout"
    echo $?
  fi
}

is_question () {
  test "${1: -1}" = "?"
  echo $?
}

main "$@"
