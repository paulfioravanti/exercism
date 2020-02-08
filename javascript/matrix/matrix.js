export class Matrix {
  constructor(matrixString) {
    this._rows = this.generateRows(matrixString)
    this._columns = this.transpose()
  }

  get rows() {
    return this._rows
  }

  get columns() {
    return this._columns
  }

  generateRows(matrixString) {
    return matrixString.split("\n").map(this.generateColumns)
  }

  generateColumns(rowString) {
    return rowString.split(" ").map(Number)
  }

  transpose() {
    const rows = this.rows

    return (
      rows[0].map((_column, index) =>
        rows.map(row => row[index])
      )
    )
  }
}
