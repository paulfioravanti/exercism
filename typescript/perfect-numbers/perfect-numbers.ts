type MaybeError<T> = T | never
type AddFactorReducer = (acc: number, candidateFactor: number) => number

export default abstract class PerfectNumbers {
  private static readonly GREATER: Readonly<number> = 1
  private static readonly EQUAL: Readonly<number> = 0
  private static readonly IS_NON_NATURAL_NUMBER =
    (num: number): boolean => {
      return num < 1
    }
  private static readonly IS_FACTOR =
    (candidateFactor: number, num: number): boolean => {
      return num % candidateFactor === 0
    }

  static classify(num: number): MaybeError<string> {
    if (PerfectNumbers.IS_NON_NATURAL_NUMBER(num)) {
      throw new Error("Classification is only possible for natural numbers.")
    }

    const characterisation: number =
      Math.sign(PerfectNumbers.aliquotSum(num) - num)

    switch (characterisation) {
      case PerfectNumbers.GREATER:
        return "abundant"
      case PerfectNumbers.EQUAL:
        return "perfect"
      default:
        return "deficient"
    }
  }

  private static aliquotSum(num: number): number {
    return PerfectNumbers.range(num).reduce(PerfectNumbers.addFactor(num), 0)
  }

  private static range(num: number): number[] {
    return [...Array(num + 1).keys()].slice(1, num)
  }

  private static addFactor(num: number): AddFactorReducer {
    return (acc: number, candidateFactor: number): number => {
      if (PerfectNumbers.IS_FACTOR(candidateFactor, num)) {
        acc += candidateFactor
      }
      return acc
    }
  }
}
