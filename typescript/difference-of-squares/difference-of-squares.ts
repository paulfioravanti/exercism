type SumReducer = (acc: number, num: number) => number

export default class Squares {
  static readonly SQUARE: number = 2
  private readonly numbers: number[]

  constructor(num: number) {
    this.numbers = [...Array(num + 1).keys()]
  }

  get sumOfSquares(): number {
    return this.numbers.reduce(this.sumSquare(), 0)
  }

  get squareOfSum(): number {
    return this.numbers.reduce(this.sum, 0)**Squares.SQUARE
  }

  get difference(): number {
    return this.squareOfSum - this.sumOfSquares
  }

  sumSquare(): SumReducer {
    return (acc: number, num: number): number => {
      return this.sum(acc, num**Squares.SQUARE)
    }
  }

  sum(acc: number, num: number): number {
    return acc + num
  }
}
