type MaybeError<T> = T | never

export default class CollatzConjecture {
  private static readonly INITIAL_STEPS: number = 0
  private static readonly TERMINATING_NUMBER: number = 1
  private static IS_EVEN = (n: number): boolean => n % 2 === 0
  private static N_DIV_TWO = (n: number): number => n / 2
  private static THREE_N_PLUS_ONE = (n: number): number => 3 * n + 1

  static steps(input: number): MaybeError<number> {
    if (input < 1) {
      throw new Error("Only positive numbers are allowed")
    }

    return this.calculateSteps(input, this.INITIAL_STEPS)
  }

  private static calculateSteps(input: number, steps: number): number {
    if (input === this.TERMINATING_NUMBER) {
      return steps
    }

    if (this.IS_EVEN(input)) {
      input = this.N_DIV_TWO(input)
    } else {
      input = this.THREE_N_PLUS_ONE(input)
    }

    return this.calculateSteps(input, steps + 1)
  }
}
