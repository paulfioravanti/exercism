export class Squares {
  static get SQUARE() {
    return 2
  }

  constructor(number) {
    this._numbers = [...Array(number + 1).keys()]
  }

  get numbers() {
    return this._numbers
  }

  get sumOfSquares() {
    return this.numbers.reduce(this.sumSquare(), 0)
  }

  get squareOfSum() {
    return this.numbers.reduce(this.sum, 0)**Squares.SQUARE
  }

  get difference() {
    return this.squareOfSum - this.sumOfSquares
  }

  sumSquare() {
    return (acc, number) => {
      return this.sum(acc, number**Squares.SQUARE)
    }
  }

  sum(acc, number) {
    return acc + number
  }
}
