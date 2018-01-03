# frozen_string_literal: true

module Atbash
  ALPHABET = ("a".."z").to_a.join
  private_constant :ALPHABET
  NON_LETTERS = /[[[:blank:]][[:punct:]]]/
  private_constant :NON_LETTERS
  GROUP_LETTERS = [/(.{5})(?=.)/, "\\1 \\2"].freeze
  private_constant :GROUP_LETTERS

  module_function

  def encode(plaintext)
    plaintext
      .downcase
      .gsub(NON_LETTERS, "")
      .tr(ALPHABET, ALPHABET.reverse)
      .gsub(*GROUP_LETTERS)
  end
end
