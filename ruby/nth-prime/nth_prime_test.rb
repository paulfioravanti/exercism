require 'minitest/autorun'
require_relative 'nth_prime'

# Common test data version: 1.0.0 016d65b
class NthPrimeTest < Minitest::Test
  def test_first_prime
    assert_equal 2, Prime.nth(1)
  end

  def test_second_prime
    assert_equal 3, Prime.nth(2)
  end

  def test_sixth_prime
    assert_equal 13, Prime.nth(6)
  end

  def test_big_prime
    assert_equal 104743, Prime.nth(10001)
  end

  def test_there_is_no_zeroth_prime
    assert_raises(ArgumentError) { Prime.nth(0) }
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
    assert_equal 1, BookKeeping::VERSION
  end
end
