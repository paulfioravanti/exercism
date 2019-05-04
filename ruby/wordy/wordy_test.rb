require 'minitest/autorun'
require 'minitest/focus'
require_relative 'wordy'

# Common test data version: 1.2.0 86d0069
class WordyTest < Minitest::Test
  def test_addition
    problem = WordProblem.new("What is 1 plus 1?")
    assert_equal(2, problem.answer)
  end

  def test_more_addition
    problem = WordProblem.new("What is 53 plus 2?")
    assert_equal(55, problem.answer)
  end

  def test_addition_with_negative_numbers
    problem = WordProblem.new("What is -1 plus -10?")
    assert_equal(-11, problem.answer)
  end

  def test_large_addition
    problem = WordProblem.new("What is 123 plus 45678?")
    assert_equal(45801, problem.answer)
  end

  def test_subtraction
    problem = WordProblem.new("What is 4 minus -12?")
    assert_equal(16, problem.answer)
  end

  def test_multiplication
    problem = WordProblem.new("What is -3 multiplied by 25?")
    assert_equal(-75, problem.answer)
  end

  def test_division
    problem = WordProblem.new("What is 33 divided by -3?")
    assert_equal(-11, problem.answer)
  end

  def test_multiple_additions
    problem = WordProblem.new("What is 1 plus 1 plus 1?")
    assert_equal(3, problem.answer)
  end

  def test_addition_and_subtraction
    problem = WordProblem.new("What is 1 plus 5 minus -2?")
    assert_equal(8, problem.answer)
  end

  def test_multiple_subtraction
    problem = WordProblem.new("What is 20 minus 4 minus 13?")
    assert_equal(3, problem.answer)
  end

  def test_subtraction_then_addition
    problem = WordProblem.new("What is 17 minus 6 plus 3?")
    assert_equal(14, problem.answer)
  end

  def test_multiple_multiplication
    problem = WordProblem.new("What is 2 multiplied by -2 multiplied by 3?")
    assert_equal(-12, problem.answer)
  end

  def test_addition_and_multiplication
    problem = WordProblem.new("What is -3 plus 7 multiplied by -2?")
    message = "You should ignore order of precedence. -3 + 7 * -2 = -8, not #{problem.answer}"
    assert_equal(-8, problem.answer, message)
  end

  def test_multiple_division
    problem = WordProblem.new("What is -12 divided by 2 divided by -3?")
    assert_equal(2, problem.answer)
  end

  def test_unknown_operation
    problem = WordProblem.new("What is 52 cubed?")
    assert_raises(ArgumentError) do
      problem.answer
    end
  end

  def test_non_math_question
    problem = WordProblem.new("Who is the President of the United States?")
    assert_raises(ArgumentError) do
      problem.answer
    end
  end
end
