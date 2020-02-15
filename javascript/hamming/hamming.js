export const compute = (left, right) => {
  if (left === right) {
    return 0
  }
  validiateStrandLengths(left, right)

  return (
    left
    .split("")
    .filter(isDifferent(right))
    .length
  )
}

function validiateStrandLengths(left, right) {
  if (left === "") {
    throw new Error("left strand must not be empty")
  }

  if (right === "") {
    throw new Error("right strand must not be empty")
  }

  if (left.length !== right.length) {
    throw new Error("left and right strands must be of equal length")
  }
}

function isDifferent(right) {
  return (nucleotide, index) => nucleotide !== right[index]
}
