# frozen_string_literal: true

module PerfectNumber
  EQUAL = 0
  private_constant :EQUAL
  GREATER = 1
  private_constant :GREATER

  module_function

  def classify(number)
    raise RuntimeError unless number.positive?

    case aliquot_sum(number) <=> number
    when GREATER
      "abundant"
    when EQUAL
      "perfect"
    else
      "deficient"
    end
  end

  def aliquot_sum(number)
    (1...number)
      .each
      .with_object(number)
      .each_with_object([], &method(:add_factor))
      .sum
  end
  private_class_method :aliquot_sum

  def add_factor((candidate_factor, number), acc)
    acc << candidate_factor if factor?(candidate_factor, number)
  end
  private_class_method :add_factor

  def factor?(candidate_factor, number)
    number
      .modulo(candidate_factor)
      .zero?
  end
  private_class_method :factor?
end
