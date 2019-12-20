#!/usr/bin/env bash

declare -r EARTH_ORBITAL_PERIOD=31557600.0
declare -rA ORBITAL_FACTORS=(
  ["Earth"]=1.0
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
  if [[ -z $factor ]]; then
    echo "not a planet"
    exit 1
  fi
  printf "%.2f" "$(space_age "$factor" "$2")"
}

space_age () {
  bc <<< "scale=3; $2 / $EARTH_ORBITAL_PERIOD / $1"
}

main "$@"
