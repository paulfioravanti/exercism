#!/usr/bin/env bash

declare -A RESISTOR_VALUES=(
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
  resistance=""

  for color in $1 $2; do
    local value=${RESISTOR_VALUES[${color}]}

    if [[ -z $value ]]; then
      echo "invalid color"
      exit 1
    fi

    resistance+=$value
  done

  echo "${resistance}"
}

main "$@"
