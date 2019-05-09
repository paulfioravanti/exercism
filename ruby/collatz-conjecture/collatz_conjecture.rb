module CollatzConjecture
  INITIAL_STEPS = 0
  private_constant :INITIAL_STEPS
  N_DIV_TWO = ->(number) { number / 2 }
  private_constant :N_DIV_TWO
  TERMINATING_NUMBER = 1
  private_constant :TERMINATING_NUMBER
  THREE_N_PLUS_ONE = ->(number) { number * 3 + 1 }
  private_constant :THREE_N_PLUS_ONE

  module_function

  def steps(number)
    raise ArgumentError unless number.positive?

    steps = INITIAL_STEPS
    return steps if number == TERMINATING_NUMBER

    loop do
      number =
        number.even? ? N_DIV_TWO.call(number) : THREE_N_PLUS_ONE.call(number)
      steps = steps.next
      return steps if number == TERMINATING_NUMBER
    end
  end
end
