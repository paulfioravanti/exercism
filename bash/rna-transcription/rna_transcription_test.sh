#!/usr/bin/env bash

# local version: 1.3.0.0

@test "Empty RNA sequence" {
  run bash rna_transcription.sh
  [[ $status -eq 0 ]]
  [[ -z $output ]]
}

@test "RNA complement of cytosine is guanine" {
  run bash rna_transcription.sh C
  [[ $status -eq 0 ]]
  [[ $output == "G" ]]
}

@test "RNA complement of guanine is cytosine" {
  run bash rna_transcription.sh G
  [[ $status -eq 0 ]]
  [[ $output == "C" ]]
}

@test "RNA complement of thymine is adenine" {
  run bash rna_transcription.sh T
  [[ $status -eq 0 ]]
  [[ $output == "A" ]]
}

@test "RNA complement of adenine is uracil" {
  run bash rna_transcription.sh A
  [[ $status -eq 0 ]]
  [[ $output == "U" ]]
}

@test "RNA complement" {
  run bash rna_transcription.sh ACGTGGTCTTAA
  [[ $status -eq 0 ]]
  [[ $output == "UGCACCAGAAUU" ]]
}

@test "Handles invalid character" {
  run bash rna_transcription.sh ACGTXCTTA
  [[ $status -eq 1 ]]
  [[ $output == "Invalid nucleotide detected." ]]
}

@test "Handles completely invalid string" {
  run bash rna_transcription.sh XXXX
  [[ $status -eq 1 ]]
  [[ $output == "Invalid nucleotide detected." ]]
}

@test "Handles partially invalid string" {
  run bash rna_transcription.sh ACGTXCTTAA
  [[ $status -eq 1 ]]
  [[ $output == "Invalid nucleotide detected." ]]
}
