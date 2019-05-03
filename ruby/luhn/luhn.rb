# frozen_string_literal: true

module Luhn
  CHECK_IF_EVENLY_DIVISIBLE_BY_10 = ->(number) { (number % 10).zero? }
  private_constant :CHECK_IF_EVENLY_DIVISIBLE_BY_10
  CONTAINS_NON_DIGITS = ->(string) { string.match?(/\D/) }
  private_constant :CONTAINS_NON_DIGITS
  # NOTE: to_i here will ensure that any potential `nil` values convert to 0
  DOUBLE = ->(number) { number.to_i * 2 }
  private_constant :DOUBLE
  INVALID_LENGTH = ->(string) { string.length < 2 }
  private_constant :INVALID_LENGTH
  STRIP_SPACES = ->(string) { string.gsub(/\s/, "") }
  private_constant :STRIP_SPACES
  SUBTRACT_9_IF_GREATER_THAN_9 = ->(number) { number > 9 ? number - 9 : number }
  private_constant :SUBTRACT_9_IF_GREATER_THAN_9

  module_function

  def valid?(string)
    stripped_string = STRIP_SPACES.call(string)
    return false if
      CONTAINS_NON_DIGITS.call(stripped_string) ||
      INVALID_LENGTH.call(stripped_string)

    stripped_string
      .then(&method(:convert_string_to_reversed_numbers))
      .each_slice(2)
      .reduce([], &method(:append_calculated_pair))
      .sum
      .then(&CHECK_IF_EVENLY_DIVISIBLE_BY_10)
  end

  def convert_string_to_reversed_numbers(string)
    string.reverse.chars.map(&:to_i)
  end
  private_class_method :convert_string_to_reversed_numbers

  def append_calculated_pair(acc, (left_value, right_value))
    calculated_right_value =
      right_value
      .then(&DOUBLE)
      .then(&SUBTRACT_9_IF_GREATER_THAN_9)

    acc + [left_value, calculated_right_value]
  end
  private_class_method :append_calculated_pair
end
