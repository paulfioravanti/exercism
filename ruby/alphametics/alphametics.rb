# frozen_string_literal: true

module Alphametics
  NON_UPPERCASE_CHARACTERS = /[^A-Z]/.freeze
  private_constant :NON_UPPERCASE_CHARACTERS
  SINGLE_DIGITS = (0..9).to_a
  private_constant :SINGLE_DIGITS

  module_function

  def solve(input)
    letters = input.gsub(NON_UPPERCASE_CHARACTERS, "").chars.uniq
    SINGLE_DIGITS.permutation(letters.length).each do |permutation|
      expression = input.tr(letters.join, permutation.join).split
      next if leading_zero?(expression)
      return letters.zip(permutation).to_h if evaluate(expression)
    end
    {}
  end

  def leading_zero?(expression)
    expression.any? { |component| component.start_with?("0") }
  end
  private_class_method :leading_zero?

  def evaluate(expression)
    expression
      .each_slice(2)
      .reduce(expression.shift.to_i, &method(:accumulate_expression))
  end
  private_class_method :evaluate

  def accumulate_expression(object, (method, value))
    object.public_send(method, value.to_i)
  end
  private_class_method :accumulate_expression
end
