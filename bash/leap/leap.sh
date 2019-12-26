#!/usr/bin/env bash

declare -ri LEAP_YEAR=4
declare -ri CENTURIAL_YEAR=100
declare -ri LEAP_CYCLE_LENGTH="$LEAP_YEAR * $CENTURIAL_YEAR"
declare -r YEAR="^[0-9]{4}$"

main () {
  check_arguments "$@"

  local -r year=$1

  if leap && (centurial || leap_cycle); then
    echo true
  else
    echo false
  fi
}

check_arguments () {
  if [[ ! "$*" =~ $YEAR ]]; then
    echo "Usage: leap.sh <year>"
    exit 1
  fi
}

leap () {
  [[ "$year % $LEAP_YEAR" -eq 0 ]]
}

centurial () {
  [[ "$year % $CENTURIAL_YEAR" -ne 0 ]]
}

leap_cycle () {
  [[ "$year % $LEAP_CYCLE_LENGTH" -eq 0 ]]
}

main "$@"
