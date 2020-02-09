type MaybeNumber = number | never

export default class CollatzConjecture {
  private static INITIAL_STEPS = 0
  private static TERMINATING_NUMBER = 1
  private static IS_EVEN = (n: number): boolean => n % 2 === 0
  private static N_DIV_TWO = (n: number): number => n / 2
  private static THREE_N_PLUS_ONE = (n: number): number => 3 * n + 1

  static steps(input: number): MaybeNumber {
    if (input < 1) {
      throw new Error("Only positive numbers are allowed")
    }

    return (
      CollatzConjecture.calculateSteps(input, CollatzConjecture.INITIAL_STEPS)
    )
  }

  private static calculateSteps(input: number, steps: number): number {
    if (input === CollatzConjecture.TERMINATING_NUMBER) {
      return steps
    }

    if (CollatzConjecture.IS_EVEN(input)) {
      input = CollatzConjecture.N_DIV_TWO(input)
    } else {
      input = CollatzConjecture.THREE_N_PLUS_ONE(input)
    }

    return CollatzConjecture.calculateSteps(input, steps + 1)
  }
}
