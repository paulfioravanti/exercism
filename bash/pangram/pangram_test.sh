#!/usr/bin/env bash

# local version: 2.0.0.0

# Check if the given string is a pangram

@test "empty sentence" {
  run bash pangram.sh ""
  [[ $status -eq 0 ]]
  [[ $output == "false" ]]
}

@test "perfect lower case" {
  run bash pangram.sh "abcdefghijklmnopqrstuvwxyz"
  [[ $status -eq 0 ]]
  [[ $output == "true" ]]
}

@test "only lower case" {
  run bash pangram.sh "the quick brown fox jumps over the lazy dog"
  echo "$output"
  [[ $status -eq 0 ]]
  [[ $output == "true" ]]
}

@test "missing the letter 'x'" {
  run bash pangram.sh "a quick movement of the enemy will jeopardize five gunboats"
  [[ $status -eq 0 ]]
  [[ $output == "false" ]]
}

@test "missing the letter 'h'" {
  run bash pangram.sh "five boxing wizards jump quickly at it"
  [[ $status -eq 0 ]]
  [[ $output == "false" ]]
}

@test "with underscores" {
  run bash pangram.sh "the_quick_brown_fox_jumps_over_the_lazy_dog"
  [[ $status -eq 0 ]]
  [[ $output == "true" ]]
}

@test "with numbers" {
  run bash pangram.sh "the 1 quick brown fox jumps over the 2 lazy dogs"
  [[ $status -eq 0 ]]
  [[ $output == "true" ]]
}

@test "missing letters replaced by numbers" {
  run bash pangram.sh "7h3 qu1ck brown fox jumps ov3r 7h3 lazy dog"
  echo "$output"
  [[ $status -eq 0 ]]
  [[ $output == "false" ]]
}

@test "mixed case and punctuation" {
  run bash pangram.sh "\"Five quacking Zephyrs jolt my wax bed.\""
  [[ $status -eq 0 ]]
  [[ $output == "true" ]]
}

@test "case insensitive" {
  run bash pangram.sh "the quick brown fox jumps over with lazy FX"
  [[ $status -eq 0 ]]
  [[ $output == "false" ]]
}
