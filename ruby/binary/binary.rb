module Binary
  BINARY_STRING = /\A[01]+\z/
  private_constant :BINARY_STRING
  BASE = 2
  private_constant :BASE
  DECIMAL_SEED = 0
  private_constant :DECIMAL_SEED

  module_function

  def to_decimal(string)
    raise ArgumentError unless string.match?(BINARY_STRING)
    string.chars.reverse.each.with_index.reduce(
      DECIMAL_SEED, &method(:add_decimal_value)
    )
  end

  def add_decimal_value(decimal, (binary_number, index))
    decimal + binary_number.to_i * BASE**index
  end
  private_class_method :add_decimal_value
end

module BookKeeping
  VERSION = 3
end
