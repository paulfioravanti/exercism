class SumOfMultiples
  RANGE = ->(limit) { (1...limit) }
  private_constant :RANGE

  def initialize(*multiples)
    @multiples = multiples.freeze
  end

  def to(limit)
    RANGE.call(limit).select { |i| multiples.any? { |j| (i % j).zero? } }.sum
  end

  private

  attr_reader :multiples
end

module BookKeeping
  VERSION = 2
end
