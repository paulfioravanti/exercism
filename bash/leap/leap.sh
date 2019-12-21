#!/usr/bin/env bash

declare -ri LEAP_YEAR=4
declare -ri CENTURIAL_YEAR=100
declare -r YEAR="^[0-9]{4}$"

main () {
  if [[ ! "$*" =~ $YEAR ]]; then
    usage
  fi

  if [[
    $(leap "$1") -eq 0 &&
    (
      $(centurial "$1") -ne 0 ||
      $(leap_cycle "$1") -eq 0
    )
  ]]
  then
    echo true
  else
    echo false
  fi
}

usage () {
  echo "Usage: leap.sh <year>"
  exit 1
}

leap () {
  echo "$1" % $LEAP_YEAR
}

centurial () {
  echo "$1" % $CENTURIAL_YEAR
}

leap_cycle () {
  local -ri leap_cycle_length="$LEAP_YEAR * $CENTURIAL_YEAR"
  echo "$1" % "$leap_cycle_length"
}


main "$@"
