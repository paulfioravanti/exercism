export default class Matrix {
  readonly rows: number[][]
  readonly columns: number[][]

  constructor(matrix: string) {
    this.rows = this.generateRows(matrix)
    this.columns = this.transpose()
  }

  private generateRows(matrix: string): number[][] {
    return matrix.split("\n").map(this.parseRowColumns)
  }

  private parseRowColumns(row: string): number[] {
    return row.split(" ").map(Number)
  }

  private transpose(): number[][] {
    const rows: number[][] = this.rows

    return (
      rows[0].map((_column: number, index: number): number[] =>
        rows.map((row: number[]): number => row[index])
      )
    )
  }
}
