require 'minitest/autorun'
require_relative 'ocr_numbers'

# Common test data version: 1.2.0 965ecad
class OcrNumbersTest < Minitest::Test
  def test_recognizes_0
    input = [" _ ",
             "| |",
             "|_|",
             "   "].join("\n")
    assert_equal "0", OcrNumbers.convert(input)
  end

  def test_recognizes_1
    input = ["   ",
             "  |",
             "  |",
             "   "].join("\n")
    assert_equal "1", OcrNumbers.convert(input)
  end

  def test_unreadable_but_correctly_sized_inputs_return_question_mark
    input = ["   ",
             "  _",
             "  |",
             "   "].join("\n")
    assert_equal "?", OcrNumbers.convert(input)
  end

  def test_input_with_a_number_of_lines_that_is_not_a_multiple_of_four_raises_an_error
    input = [" _ ",
             "| |",
             "   "].join("\n")
    assert_raises(ArgumentError) do
      OcrNumbers.convert(input)
    end
  end

  def test_input_with_a_number_of_columns_that_is_not_a_multiple_of_three_raises_an_error
    input = ["    ",
             "   |",
             "   |",
             "    "].join("\n")
    assert_raises(ArgumentError) do
      OcrNumbers.convert(input)
    end
  end

  def test_recognizes_110101100
    input = ["       _     _        _  _ ",
             "  |  || |  || |  |  || || |",
             "  |  ||_|  ||_|  |  ||_||_|",
             "                           "].join("\n")
    assert_equal "110101100", OcrNumbers.convert(input)
  end

  def test_garbled_numbers_in_a_string_are_replaced_with_question_mark
    input = ["       _     _           _ ",
             "  |  || |  || |     || || |",
             "  |  | _|  ||_|  |  ||_||_|",
             "                           "].join("\n")
    assert_equal "11?10?1?0", OcrNumbers.convert(input)
  end

  def test_recognizes_2
    input = [" _ ",
             " _|",
             "|_ ",
             "   "].join("\n")
    assert_equal "2", OcrNumbers.convert(input)
  end

  def test_recognizes_3
    input = [" _ ",
             " _|",
             " _|",
             "   "].join("\n")
    assert_equal "3", OcrNumbers.convert(input)
  end

  def test_recognizes_4
    input = ["   ",
             "|_|",
             "  |",
             "   "].join("\n")
    assert_equal "4", OcrNumbers.convert(input)
  end

  def test_recognizes_5
    input = [" _ ",
             "|_ ",
             " _|",
             "   "].join("\n")
    assert_equal "5", OcrNumbers.convert(input)
  end

  def test_recognizes_6
    input = [" _ ",
             "|_ ",
             "|_|",
             "   "].join("\n")
    assert_equal "6", OcrNumbers.convert(input)
  end

  def test_recognizes_7
    input = [" _ ",
             "  |",
             "  |",
             "   "].join("\n")
    assert_equal "7", OcrNumbers.convert(input)
  end

  def test_recognizes_8
    input = [" _ ",
             "|_|",
             "|_|",
             "   "].join("\n")
    assert_equal "8", OcrNumbers.convert(input)
  end

  def test_recognizes_9
    input = [" _ ",
             "|_|",
             " _|",
             "   "].join("\n")
    assert_equal "9", OcrNumbers.convert(input)
  end

  def test_recognizes_string_of_decimal_numbers
    input = ["    _  _     _  _  _  _  _  _ ",
             "  | _| _||_||_ |_   ||_||_|| |",
             "  ||_  _|  | _||_|  ||_| _||_|",
             "                              "].join("\n")
    assert_equal "1234567890", OcrNumbers.convert(input)
  end

  def test_numbers_separated_by_empty_lines_are_recognized_lines_are_joined_by_commas
    input = ["    _  _ ",
             "  | _| _|",
             "  ||_  _|",
             "         ",
             "    _  _ ",
             "|_||_ |_ ",
             "  | _||_|",
             "         ",
             " _  _  _ ",
             "  ||_||_|",
             "  ||_| _|",
             "         "].join("\n")
    assert_equal "123,456,789", OcrNumbers.convert(input)
  end
end
