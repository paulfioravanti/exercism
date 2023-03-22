# frozen_string_literal: true

module Luhn
  TWO_OR_MORE_DIGITS = /\A\d{2,}\z/.freeze
  private_constant :TWO_OR_MORE_DIGITS

  module_function

  def valid?(string)
    string = string.gsub(/\s/, "")
    return false unless string.match?(TWO_OR_MORE_DIGITS)

    string
      .then(&method(:convert_to_reversed_numbers))
      .each_slice(2)
      .sum(&method(:calculate_pair))
      .then(&method(:divisible_by_ten?))
  end

  private_class_method def convert_to_reversed_numbers(string)
    string.reverse.chars.map(&:to_i)
  end

  private_class_method def calculate_pair((first, second))
    return first if second.blank?

    second =
      second
      .then { |number| number * 2 }
      .then { |number| number > 9 ? number - 9 : number }

    first + second
  end

  private_class_method def divisible_by_ten?(number)
    number.modulo(10).zero?
  end
end
