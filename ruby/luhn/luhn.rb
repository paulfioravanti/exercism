# frozen_string_literal: true

module Luhn
  MIN_DIGITS = 2
  private_constant :MIN_DIGITS

  module_function

  def valid?(string)
    return false if
      (string = string.gsub(/\s/, "")).match?(/\D/) ||
      (numbers = to_numbers(string)).size < MIN_DIGITS
    numbers
      .each_slice(2)
      .reduce([], &method(:convert))
      .sum
      .yield_self { |sum| (sum % 10).zero? }
  end

  def to_numbers(number)
    number.reverse.chars.map(&:to_i)
  end
  private_class_method :to_numbers

  def convert(acc, (left, right))
    acc + [left, (product = right.to_i * 2) > 9 ? product - 9 : product]
  end
  private_class_method :convert
end

module BookKeeping
  VERSION = 1
end
