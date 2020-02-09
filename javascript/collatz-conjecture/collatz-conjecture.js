const INITIAL_STEPS = 0
const TERMINATING_NUMBER = 1
const IS_EVEN = number => number % 2 === 0
const N_DIV_TWO = n => n / 2
const THREE_N_PLUS_ONE = n => 3 * n + 1

export const steps = input => {
  if (input < 1) {
    throw new Error("Only positive numbers are allowed")
  }

  return calculateSteps(input, INITIAL_STEPS)
}

function calculateSteps(input, steps) {
  if (input === TERMINATING_NUMBER) {
    return steps
  }

  input = IS_EVEN(input) ? N_DIV_TWO(input) : THREE_N_PLUS_ONE(input)
  return calculateSteps(input, steps + 1)
}
