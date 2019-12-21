#!/usr/bin/env bash

declare -rA RESISTOR_VALUES=(
  ["black"]=0
  ["brown"]=1
  ["red"]=2
  ["orange"]=3
  ["yellow"]=4
  ["green"]=5
  ["blue"]=6
  ["violet"]=7
  ["grey"]=8
  ["white"]=9
)

main () {
  local resistance=""

  for color in $1 $2; do
    add_color_value
  done

  echo "$resistance"
}

add_color_value () {
  local value=${RESISTOR_VALUES[$color]}
  check_value
  resistance+=$value
}

check_value () {
  if [[ -z $value ]]; then
    echo "invalid color"
    exit 1
  fi
}

main "$@"
