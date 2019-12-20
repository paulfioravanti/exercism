#!/usr/bin/env bash

# local version: 1.2.0.0

@test "age on Earth" {
    expected=31.69
    run bash space_age.sh "Earth" 1000000000
    [[ $status -eq 0 ]]
    [[ $output == "$expected" ]]
}

@test "age on Mercury" {
    expected=280.88
    run bash space_age.sh "Mercury" 2134835688
    [[ $status -eq 0 ]]
    [[ $output == "$expected" ]]
}

@test "age on Venus" {
    expected=9.78
    run bash space_age.sh "Venus" 189839836
    [[ $status -eq 0 ]]
    [[ $output == "$expected" ]]
}

@test "age on Mars" {
    expected=35.88
    run bash space_age.sh "Mars" 2129871239
    [[ $status -eq 0 ]]
    [[ $output == "$expected" ]]
}

@test "age on Jupiter" {
    expected=2.41
    run bash space_age.sh "Jupiter" 901876382
    [[ $status -eq 0 ]]
    [[ $output == "$expected" ]]
}

@test "age on Saturn" {
    expected=2.15
    run bash space_age.sh "Saturn" 2000000000
    [[ $status -eq 0 ]]
    [[ $output == "$expected" ]]
}

@test "age on Uranus" {
    expected=0.46
    run bash space_age.sh "Uranus" 1210123456
    [[ $status -eq 0 ]]
    [[ $output == "$expected" ]]
}

@test "age on Neptune" {
    expected=0.35
    run bash space_age.sh "Neptune" 1821023456
    [[ $status -eq 0 ]]
    [[ $output == "$expected" ]]
}

@test "not a planet" {
    expected="not a planet"
    run bash space_age.sh "Pluto" 1821023456
    [[ $status -eq 1 ]]
    [[ $output == *"$expected"* ]]
}
