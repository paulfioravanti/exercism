type Row = number[]
type PascalsTriangle = Array<Row>
type NumberMapper = (num: number, index: number) => number
type BinomialFunction = (exponent: number) => number

export default class Triangle {
  readonly rows: PascalsTriangle

  constructor(num: number) {
    this.rows = this.range(0, num - 1).map(this.generateRow.bind(this))
  }

  get lastRow(): Row {
    return this.rows[this.rows.length - 1]
  }

  private range(start: number, end: number): number[] {
    return Array.from({ length: end - start + 1 }, this.sum(start))
  }

  private sum(start: number): NumberMapper {
    return (_num: number, index: number): number => {
      return start + index
    }
  }

  private generateRow(rowNum: number): number[] {
    return this.range(0, rowNum).map(this.binomial(rowNum))
  }

  // https://en.wikipedia.org/wiki/Binomial_theorem
  // "n (rowNum) choose k (exponent)" => n!/(n - k)!k!
  private binomial(rowNum: number): BinomialFunction {
    return (exponent: number): number => {
      return (
        this.factorial(rowNum) /
        (this.factorial(rowNum - exponent) * this.factorial(exponent))
      )
    }
  }

  private factorial(num: number): number {
    if (num === 0) {
      return 1
    }

    return this.range(1, num).reduce(this.product, 1)
  }

  private product(acc: number, num: number): number {
    return acc * num
  }
}
