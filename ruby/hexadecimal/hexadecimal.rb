# frozen_string_literal: true

class Hexadecimal
  HEX_DIGITS = "0123456789ABCDEF"
  private_constant :HEX_DIGITS
  HEX_STRING = /\A[#{HEX_DIGITS}]+\z/
  private_constant :HEX_STRING
  BASE = 16
  private_constant :BASE

  def initialize(string)
    @string = string.upcase
  end

  def to_decimal
    return 0 unless string.match?(HEX_STRING)
    string.chars.reverse.each.with_index.sum(&method(:hex_to_dec))
  end

  private

  attr_reader :string

  def hex_to_dec((hex_character, exponent))
    HEX_DIGITS.index(hex_character) * BASE**exponent
  end
end
