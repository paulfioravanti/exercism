class Triangle
  # https://en.wikipedia.org/wiki/Pascal%27s_triangle
  # "The rows of Pascal's triangle are conventionally enumerated
  # starting with row n = 0 at the top (the 0th row)", so instantly decrement
  # the number of rows by 1.
  def initialize(num_rows)
    @max_row = num_rows - 1
  end

  def rows
    (0..max_row).map(&method(:generate_row))
  end

  private

  attr_reader :max_row

  def generate_row(row_num)
    (0..row_num)
      .each
      .with_object(row_num)
      .map(&method(:binomial))
  end

  # https://en.wikipedia.org/wiki/Binomial_theorem
  # "n (row_num) choose k (exponent)" => n!/(n - k)!k!
  def binomial(exponent, row_num)
    factorial(row_num) / (factorial(row_num - exponent) * factorial(exponent))
  end

  # Provide starting accumulator for 0! == 1
  def factorial(num)
    (1..num).reduce(1, :*)
  end
end
