type PowerReducer = (acc: number, digit: number) => number

export default abstract class ArmstrongNumbers {
  static isArmstrongNumber(num: number): boolean {
    const sumOfPowers: number =
      ArmstrongNumbers.sumPowers(ArmstrongNumbers.digits(num))

    return num === sumOfPowers
  }

  private static digits(num: number): number[] {
    return Array.from(num.toString()).map(Number)
  }

  private static sumPowers(digits: number[]): number {
    return digits.reduce(ArmstrongNumbers.power(digits.length), 0)
  }

  private static power(length: number): PowerReducer {
    return (acc: number, digit: number): number => {
      return acc + Math.pow(digit, length)
    }
  }
}
