class SumOfMultiples
  LIMIT_TO_RANGE = ->(limit) { (1...limit) }
  private_constant :LIMIT_TO_RANGE

  def initialize(*numbers)
    @numbers = numbers.freeze
  end

  def to(limit)
    limit
      .then(&LIMIT_TO_RANGE)
      .select(&method(:multiples?))
      .sum
  end

  private

  attr_reader :numbers

  def multiples?(range_number)
    numbers.any? { |number| multiple?(range_number, number) }
  end

  def multiple?(number1, number2)
    (number1 % number2).zero?
  end
end
