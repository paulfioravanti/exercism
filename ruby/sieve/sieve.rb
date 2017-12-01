class Sieve
  def initialize(limit)
    @limit = limit
    @range = Hash[(2..limit).map { |number| [number, :prime] }].freeze
  end

  def primes
    mark_composites.select { |_key, value| value == :prime }.keys
  end

  private

  attr_reader :limit, :range

  def mark_composites
    (2..Math.sqrt(limit)).reduce(range.dup) do |list, number|
      next(list) if list[number] == :composite
      mark_multiples_as_composites(list, number)
    end
  end

  def mark_multiples_as_composites(list, number)
    multiples(number).each_with_object(list) do |multiple, numbers|
      numbers[multiple] = :composite
    end
  end

  def multiples(number)
    (number**2).step(by: number, to: limit)
  end
end

module BookKeeping
  VERSION = 1
end
