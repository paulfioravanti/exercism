export const compute = (left, right) => {
  if (left === right) {
    return 0
  }

  validiateStrandLengths(left, right)
  return zip(left, right).reduce(countDifference, 0)
}

function validiateStrandLengths(left, right) {
  if (left.length === 0) {
    throw new Error("left strand must not be empty")
  }

  if (right.length === 0) {
    throw new Error("right strand must not be empty")
  }

  if (left.length !== right.length) {
    throw new Error("left and right strands must be of equal length")
  }
}

function zip(left, right) {
  return left.split("").map(pair(right))
}

function pair(right) {
  return (element, index) => [element, right[index]]
}

function countDifference(acc, [leftNucleotide, rightNucleotide]) {
  return leftNucleotide !== rightNucleotide ? acc + 1 : acc
}
