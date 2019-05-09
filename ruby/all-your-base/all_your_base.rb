module BaseConverter
  INITIAL_SUM = 0
  private_constant :INITIAL_SUM
  MINIMUM_BASE = 2
  private_constant :MINIMUM_BASE

  module_function

  def convert(input_base, digits, output_base)
    raise ArgumentError if
      invalid_bases?([input_base, output_base]) ||
      invalid_digits?(digits, input_base)

    total = sum_input(digits, input_base)
    convert_to_output_base(total, output_base)
  end

  def invalid_bases?(bases)
    bases.any? { |base| base < MINIMUM_BASE }
  end
  private_class_method :invalid_bases?

  def invalid_digits?(digits, input_base)
    digits.any?(&:negative?) ||
      digits.any? { |digit| digit >= input_base }
  end
  private_class_method :invalid_digits?

  def sum_input(digits, input_base)
    digits
      .reverse
      .each
      .with_index
      .with_object(input_base)
      .reduce(INITIAL_SUM, &method(:add_power))
  end
  private_class_method :sum_input

  def add_power(acc, ((digit, index), input_base))
    acc + digit * input_base**index
  end
  private_class_method :add_power

  def convert_to_output_base(total, output_base)
    conversion = []
    while total >= output_base
      conversion.append(total % output_base)
      total /= output_base
    end

    conversion
      .append(total % output_base)
      .reverse
  end
  private_class_method :convert_to_output_base
end
