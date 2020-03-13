// https://en.wikipedia.org/wiki/Pascal%27s_triangle
// "The rows of Pascal's triangle are conventionally enumerated
// starting with row n = 0 at the top (the 0th row)", so instantly decrement
// the number of rows by 1.
export const rows = num => {
  return range(0, num - 1).map(generateRow)
}

function range(start, end) {
  return Array(end - start + 1).fill().map(sum(start))
}

function sum(start) {
  return (_num, index) => {
    return start + index
  }
}

function generateRow(rowNum) {
  return range(0, rowNum).map(binomial(rowNum))
}

// https://en.wikipedia.org/wiki/Binomial_theorem
// "n (rowNum) choose k (exponent)" => n!/(n - k)!k!
function binomial(rowNum) {
  return (exponent) => {
    return (
      factorial(rowNum) / (factorial(rowNum - exponent) * factorial(exponent))
    )
  }
}

function factorial(num) {
  if (num === 0) {
    return 1
  }

  return range(1, num).reduce(product, 1)
}

function product(acc, num) {
  return acc * num
}
