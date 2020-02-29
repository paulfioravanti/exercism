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
  const paddedRow = paddedRows[0]
  const finalIndex = paddedRow.length - 1
  return paddedRow.map(transposeRow(paddedRows, finalIndex))
}

function transposeRow(paddedRows, finalIndex) {
  return (_column, index) => {
    const row = paddedRows.map(row => row[index]).join("")
    return index === finalIndex ? row.trimEnd() : row
  }
}
