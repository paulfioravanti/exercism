# frozen_string_literal: true

module Luhn
  TWO_OR_MORE_DIGITS_ONLY = ->(string) { string.match?(/\A\d{2,}\z/) }
  private_constant :TWO_OR_MORE_DIGITS_ONLY
  CHECK_IF_EVENLY_DIVISIBLE_BY_10 = ->(number) { (number % 10).zero? }
  private_constant :CHECK_IF_EVENLY_DIVISIBLE_BY_10
  # NOTE: to_i here will ensure that any potential `nil` values convert to 0
  DOUBLE = ->(number) { number.to_i * 2 }
  private_constant :DOUBLE
  STRIP_SPACES = ->(string) { string.gsub(/\s/, "") }
  private_constant :STRIP_SPACES
  SUBTRACT_9_IF_GREATER_THAN_9 = ->(number) { number > 9 ? number - 9 : number }
  private_constant :SUBTRACT_9_IF_GREATER_THAN_9

  module_function

  def valid?(string)
    stripped_string = STRIP_SPACES.call(string)
    return false unless TWO_OR_MORE_DIGITS_ONLY.call(stripped_string)

    stripped_string
      .then(&method(:convert_string_to_reversed_numbers))
      .each_slice(2)
      .sum(&method(:calculate_pair))
      .then(&CHECK_IF_EVENLY_DIVISIBLE_BY_10)
  end

  def convert_string_to_reversed_numbers(string)
    string.reverse.chars.map(&:to_i)
  end
  private_class_method :convert_string_to_reversed_numbers

  def calculate_pair((left_value, right_value))
    right_luhn_value =
      right_value
      .then(&DOUBLE)
      .then(&SUBTRACT_9_IF_GREATER_THAN_9)

    left_value + right_luhn_value
  end
  private_class_method :calculate_pair
end
