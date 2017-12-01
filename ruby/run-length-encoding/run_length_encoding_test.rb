require 'minitest/autorun'
require_relative 'run_length_encoding'

# Common test data version: 1.0.0 503a57a
class RunLengthEncodingTest < Minitest::Test
  def test_encode_empty_string
    input = ''
    output = ''
    assert_equal output, RunLengthEncoding.encode(input)
  end

  def test_encode_single_characters_only_are_encoded_without_count
    input = 'XYZ'
    output = 'XYZ'
    assert_equal output, RunLengthEncoding.encode(input)
  end

  def test_encode_string_with_no_single_characters
    input = 'AABBBCCCC'
    output = '2A3B4C'
    assert_equal output, RunLengthEncoding.encode(input)
  end

  def test_encode_single_characters_mixed_with_repeated_characters
    input = 'WWWWWWWWWWWWBWWWWWWWWWWWWBBBWWWWWWWWWWWWWWWWWWWWWWWWB'
    output = '12WB12W3B24WB'
    assert_equal output, RunLengthEncoding.encode(input)
  end

  def test_encode_multiple_whitespace_mixed_in_string
    input = '  hsqq qww  '
    output = '2 hs2q q2w2 '
    assert_equal output, RunLengthEncoding.encode(input)
  end

  def test_encode_lowercase_characters
    input = 'aabbbcccc'
    output = '2a3b4c'
    assert_equal output, RunLengthEncoding.encode(input)
  end

  def test_decode_empty_string
    input = ''
    output = ''
    assert_equal output, RunLengthEncoding.decode(input)
  end

  def test_decode_single_characters_only
    input = 'XYZ'
    output = 'XYZ'
    assert_equal output, RunLengthEncoding.decode(input)
  end

  def test_decode_string_with_no_single_characters
    input = '2A3B4C'
    output = 'AABBBCCCC'
    assert_equal output, RunLengthEncoding.decode(input)
  end

  def test_decode_single_characters_with_repeated_characters
    input = '12WB12W3B24WB'
    output = 'WWWWWWWWWWWWBWWWWWWWWWWWWBBBWWWWWWWWWWWWWWWWWWWWWWWWB'
    assert_equal output, RunLengthEncoding.decode(input)
  end

  def test_decode_multiple_whitespace_mixed_in_string
    input = '2 hs2q q2w2 '
    output = '  hsqq qww  '
    assert_equal output, RunLengthEncoding.decode(input)
  end

  def test_decode_lower_case_string
    input = '2a3b4c'
    output = 'aabbbcccc'
    assert_equal output, RunLengthEncoding.decode(input)
  end

  def test_consistency_encode_followed_by_decode_gives_original_string
    input = 'zzz ZZ  zZ'
    output = 'zzz ZZ  zZ'
    assert_equal output,
                 RunLengthEncoding.decode(RunLengthEncoding.encode(input))
  end

  # Problems in exercism evolve over time, as we find better ways to ask
  # questions.
  # The version number refers to the version of the problem you solved,
  # not your solution.
  #
  # Define a constant named VERSION inside of the top level BookKeeping
  # module, which may be placed near the end of your file.
  #
  # In your file, it will look like this:
  #
  # module BookKeeping
  #   VERSION = 1 # Where the version number matches the one in the test.
  # end
  #
  # If you are curious, read more about constants on RubyDoc:
  # http://ruby-doc.org/docs/ruby-doc-bundle/UsersGuide/rg/constants.html

  def test_bookkeeping
    assert_equal 3, BookKeeping::VERSION
  end
end
