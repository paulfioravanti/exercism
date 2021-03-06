export default class Squares {
  static readonly SQUARE: Readonly<number> = 2
  private readonly numbers: Readonly<number[]>

  constructor(num: number) {
    this.numbers = [...Array(num + 1).keys()]
  }

  get sumOfSquares(): number {
    return this.numbers.reduce(this.sumSquare.bind(this), 0)
  }

  get squareOfSum(): number {
    return this.numbers.reduce(this.sum, 0)**Squares.SQUARE
  }

  get difference(): number {
    return this.squareOfSum - this.sumOfSquares
  }

  sumSquare(acc: number, num: number): number {
    return this.sum(acc, num**Squares.SQUARE)
  }

  sum(acc: number, num: number): number {
    return acc + num
  }
}
