module CollatzConjecture
  INCREMENT = 1
  private_constant :INCREMENT
  N_DIV_TWO = ->(number) { number / 2 }
  private_constant :N_DIV_TWO
  TERMINATING_NUMBER = 1
  private_constant :TERMINATING_NUMBER
  THREE_N_PLUS_ONE = ->(number) { number * 3 + 1 }
  private_constant :THREE_N_PLUS_ONE
  ZERO_STEPS = 0
  private_constant :ZERO_STEPS

  module_function

  def steps(number)
    raise ArgumentError unless number.positive?
    return ZERO_STEPS if number == TERMINATING_NUMBER

    ZERO_STEPS.then { |steps| calculate_steps(steps, number) }
  end

  def calculate_steps(steps, number)
    loop do
      number =
        if number.even?
          N_DIV_TWO.call(number)
        else
          THREE_N_PLUS_ONE.call(number)
        end
      steps += INCREMENT
      return steps if number == TERMINATING_NUMBER
    end
  end
  private_class_method :calculate_steps
end
