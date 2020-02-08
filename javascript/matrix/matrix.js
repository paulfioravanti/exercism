export class Matrix {
  constructor(matrixString) {
    this._rows = this.generateRows(matrixString)
  }

  get rows() {
    return this._rows
  }

  get columns() {
    return this.transpose(this.rows)
  }

  generateRows(matrixString) {
    return (
      matrixString
      .split("\n")
      .map(rowString => rowString.split(" "))
      .map(row => row.map(column => parseInt(column)))
    )
  }

  transpose(rows) {
    return rows[0].map((_column, index) => rows.map(row => row[index]))
  }
}
