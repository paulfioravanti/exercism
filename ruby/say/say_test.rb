require 'minitest/autorun'
require_relative 'say'

# Common test data version: 1.2.0 a0cee46
class SayTest < Minitest::Test
  def test_zero
    number = 0
    assert_equal "zero", Say.new(number).in_english
  end

  def test_one
    number = 1
    assert_equal "one", Say.new(number).in_english
  end

  def test_fourteen
    number = 14
    assert_equal "fourteen", Say.new(number).in_english
  end

  def test_twenty
    number = 20
    assert_equal "twenty", Say.new(number).in_english
  end

  def test_twenty_two
    number = 22
    assert_equal "twenty-two", Say.new(number).in_english
  end

  def test_one_hundred
    number = 100
    assert_equal "one hundred", Say.new(number).in_english
  end

  def test_one_hundred_twenty_three
    number = 123
    assert_equal "one hundred twenty-three", Say.new(number).in_english
  end

  def test_one_thousand
    number = 1_000
    assert_equal "one thousand", Say.new(number).in_english
  end

  def test_one_thousand_two_hundred_thirty_four
    number = 1_234
    assert_equal "one thousand two hundred thirty-four", Say.new(number).in_english
  end

  def test_one_million
    number = 1_000_000
    assert_equal "one million", Say.new(number).in_english
  end

  def test_one_million_two_thousand_three_hundred_forty_five
    number = 1_002_345
    assert_equal "one million two thousand three hundred forty-five", Say.new(number).in_english
  end

  def test_one_billion
    number = 1_000_000_000
    assert_equal "one billion", Say.new(number).in_english
  end

  def test_a_big_number
    number = 987_654_321_123
    assert_equal "nine hundred eighty-seven billion six hundred fifty-four million three hundred twenty-one thousand one hundred twenty-three", Say.new(number).in_english
  end

  def test_numbers_below_zero_are_out_of_range
    number = -1
    assert_raises(ArgumentError) do
      Say.new(number).in_english
    end
  end

  def test_numbers_above_999_999_999_999_are_out_of_range
    number = 1_000_000_000_000
    assert_raises(ArgumentError) do
      Say.new(number).in_english
    end
  end
end
