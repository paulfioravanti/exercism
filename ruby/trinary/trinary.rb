class Trinary
  TRINARY_STRING = /\A[012]+\z/.freeze
  private_constant :TRINARY_STRING
  BASE = 3
  private_constant :BASE
  DECIMAL_SEED = 0
  private_constant :DECIMAL_SEED
  INVALID_TRINARY = 0
  private_constant :INVALID_TRINARY

  def initialize(string)
    @string = string
  end

  def to_decimal
    return INVALID_TRINARY unless string.match?(TRINARY_STRING)

    string.chars.reverse.each.with_index.reduce(
      DECIMAL_SEED, &method(:add_decimal_value)
    )
  end

  private

  attr_reader :string

  def add_decimal_value(decimal, (trinary_number, index))
    decimal + trinary_number.to_i * BASE**index
  end
end

module BookKeeping
  VERSION = 1
end
