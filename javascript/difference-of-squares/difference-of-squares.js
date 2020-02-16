export class Squares {
  static get SQUARE() {
    return 2
  }

  constructor(number) {
    this._number = number
  }

  get number() {
    return this._number
  }

  get sumOfSquares() {
    return this.numberRange().reduce(this.addSquare(), 0)
  }

  get squareOfSum() {
    return this.numberRange().reduce(this.sum, 0)**Squares.SQUARE
  }

  get difference() {
    return this.squareOfSum - this.sumOfSquares
  }

  numberRange() {
    return [...Array(this.number).keys()].map(this.increment())
  }

  addSquare() {
    return (acc, number) => {
      return this.sum(acc, number**Squares.SQUARE)
    }
  }

  increment() {
    return (number) => {
      return this.sum(1, number)
    }
  }

  sum(acc, number) {
    return acc + number
  }
}
