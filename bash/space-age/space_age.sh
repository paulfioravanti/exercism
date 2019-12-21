#!/usr/bin/env bash

declare -ri EARTH_ORBITAL_PERIOD=31557600
declare -rA ORBITAL_FACTORS=(
  ["Earth"]=1
  ["Mercury"]=0.2408467
  ["Venus"]=0.61519726
  ["Mars"]=1.8808158
  ["Jupiter"]=11.862615
  ["Saturn"]=29.447498
  ["Uranus"]=84.016846
  ["Neptune"]=164.79132
)

main () {
  local -r factor="${ORBITAL_FACTORS[$1]}"
  check_factor
  local -ri seconds=$2
  printf "%.2f" "$(space_age)"
}

check_factor () {
  if [[ -z $factor ]]; then
    echo "not a planet"
    exit 1
  fi
}

space_age () {
  bc <<< "scale=3; $seconds / $EARTH_ORBITAL_PERIOD / $factor"
}

main "$@"
