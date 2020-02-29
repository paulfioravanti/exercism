export const transpose = rows => {
  if (rows.length < 1) {
    return rows
  }

  const maxRowWidth = calculateMaxRowWidth(rows)
  const paddedRows = rows.map(generateRightPaddedRow(maxRowWidth))
  return transposeRows(paddedRows)
}

function calculateMaxRowWidth(rows) {
  return Math.max(...rows.map(row => row.length))
}

function generateRightPaddedRow(maxRowWidth) {
  return (row) => {
    return row.padEnd(maxRowWidth).split("")
  }
}

function transposeRows(paddedRows) {
  const rows = paddedRows[0].map(transposeRow(paddedRows))
  const last = rows.pop().trimEnd()
  rows.push(last)
  return rows
}

function transposeRow(paddedRows) {
  return (_column, index) => {
    return paddedRows.map(row => row[index]).join("")
  }
}
