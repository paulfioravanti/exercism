# frozen_string_literal: true

module Atbash
  ALPHABET = ("a".."z").to_a.join
  private_constant :ALPHABET
  NON_LETTERS = /[[[:blank:]][[:punct:]]]/
  private_constant :NON_LETTERS
  TEXT_GROUPING = /(.{5})(?=.)/
  private_constant :TEXT_GROUPING
  GROUPING_OUTPUT = "\\1 \\2"
  private_constant :GROUPING_OUTPUT

  module_function

  def encode(plaintext)
    plaintext
      .downcase
      .gsub(NON_LETTERS, "")
      .tr(ALPHABET, ALPHABET.reverse)
      .gsub(TEXT_GROUPING, GROUPING_OUTPUT)
  end
end
