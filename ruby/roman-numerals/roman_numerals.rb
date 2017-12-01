class Integer
  ROMAN_NUMERALS = {
    "M" => 1000,
    "CM" => 900,
    "D" => 500,
    "CD" => 400,
    "C" => 100,
    "XC" => 90,
    "L" => 50,
    "XL" => 40,
    "X" => 10,
    "IX" => 9,
    "V" => 5,
    "IV" => 4,
    "I" => 1
  }.freeze
  private_constant :ROMAN_NUMERALS

  def to_roman
    ROMAN_NUMERALS.each_with_object(
      { string: "", number: self }, &method(:append_roman_letters)
    )[:string]
  end

  private

  def append_roman_letters((roman_letters, value), roman_numeral)
    quotient, roman_numeral[:number] = roman_numeral[:number].divmod(value)
    roman_numeral[:string] << roman_letters * quotient
  end
end

module BookKeeping
  VERSION = 2
end
