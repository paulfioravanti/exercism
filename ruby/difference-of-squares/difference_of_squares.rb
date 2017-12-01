class Squares
  SQUARE = 2
  private_constant :SQUARE

  def initialize(number)
    @sum = (1..number).method(:sum)
  end

  def square_of_sum
    sum.call**SQUARE
  end

  def sum_of_squares
    sum.call { |number| number**SQUARE }
  end

  def difference
    square_of_sum - sum_of_squares
  end

  private

  attr_reader :sum
end

module BookKeeping
  VERSION = 4
end
