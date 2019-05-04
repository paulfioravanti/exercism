class SumOfMultiples
  RANGE_START = 1
  private_constant :RANGE_START

  def initialize(*numbers)
    @numbers = numbers.freeze
  end

  def to(limit)
    (RANGE_START..limit)
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
