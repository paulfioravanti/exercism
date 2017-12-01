module Prime
  MINIMUM_PRIME = 1

  module_function

  def nth(number)
    raise ArgumentError if number < MINIMUM_PRIME
    primes.take(number).to_a.last
  end

  def primes
    2.upto(Float::INFINITY).lazy.reject { |number| composite?(number) }
  end
  private_class_method :primes

  def composite?(number)
    2.upto(Math.sqrt(number)).any? { |n| (number % n).zero? }
  end
  private_class_method :composite?
end

module BookKeeping
  VERSION = 1
end
