#!/usr/bin/env bash

# local version: 1.2.0.1

@test "no name given" {
  run bash two_fer.sh
  [[ $status -eq 0 ]]
  [[ $output == "One for you, one for me." ]]
}

@test "a name given" {
  run bash two_fer.sh Alice
  [[ $status -eq 0 ]]
  [[ $output == "One for Alice, one for me." ]]
}

@test "another name given" {
  run bash two_fer.sh Bob
  [[ $status -eq 0 ]]
  [[ $output == "One for Bob, one for me." ]]
}

# bash-specific test: Focus the student's attention on the effects of
# word splitting and filename expansion:
# https://www.gnu.org/software/bash/manual/bash.html#Shell-Expansions

@test "handle arg with spaces" {
  run bash two_fer.sh "John Smith" "Mary Ann"
  [[ $status -eq 0 ]]
  [[ $output == "One for John Smith, one for me." ]]
}

@test "handle arg with glob char" {
  run bash two_fer.sh "*"
  [[ $status -eq 0 ]]
  [[ $output == "One for *, one for me." ]]
}
