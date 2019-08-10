module ArmstrongNumbers
  module_function

  def include?(number)
    sum =
      number
      .digits
      .then(&method(:sum_powers))

    sum == number
  end

  def sum_powers(digits)
    digits
      .each
      .with_object(digits.length)
      .sum(&method(:power))
  end
  private_class_method :sum_powers

  def power((digit, length))
    digit**length
  end
  private_class_method :power
end
