# frozen_string_literal: true

module Alphametics
  NON_UPPERCASE_CHARACTERS = /[^A-Z]/
  private_constant :NON_UPPERCASE_CHARACTERS
  SINGLE_DIGITS = (0..9).to_a
  private_constant :SINGLE_DIGITS
  ZERO = "0"
  private_constant :ZERO
  METHOD_VALUE_SLICE = 2
  private_constant :METHOD_VALUE_SLICE

  module_function

  def solve(input)
    letters = input.gsub(NON_UPPERCASE_CHARACTERS, "").chars.uniq
    SINGLE_DIGITS.permutation(letters.length).each do |permutation|
      expression = input.tr(letters.join, permutation.join).split
      next if expression.any? { |component| component.start_with?(ZERO) }
      return letters.zip(permutation).to_h if evaluate(expression)
    end
    {}
  end

  def evaluate(expression)
    expression.each_slice(METHOD_VALUE_SLICE).reduce(
      expression.shift.to_i,
      &method(:accumulate_expression)
    )
  end
  private_class_method :evaluate

  def accumulate_expression(object, (method, value))
    object.public_send(method, value.to_i)
  end
  private_class_method :accumulate_expression
end

module BookKeeping
  VERSION = 4
end
