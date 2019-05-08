class Series
  INITIAL_PRODUCT = 0
  private_constant :INITIAL_PRODUCT
  NON_DIGIT = /\D/.freeze
  private_constant :NON_DIGIT
  ZERO_SERIES_PRODUCT = 1
  private_constant :ZERO_SERIES_PRODUCT

  def initialize(digits)
    @digits = digits
  end

  def largest_product(series)
    raise ArgumentError if invalid_series?(series) || invalid_digits?
    return ZERO_SERIES_PRODUCT if series.zero?

    digits
      .chars
      .map(&:to_i)
      .each_cons(series)
      .reduce(INITIAL_PRODUCT, &method(:compare_products))
  end

  private

  attr_reader :digits

  def invalid_series?(series)
    series > digits.length
  end

  def invalid_digits?
    digits.match?(NON_DIGIT)
  end

  def compare_products(acc, subseries)
    product = subseries.reduce(:*)
    product > acc ? product : acc
  end
end
