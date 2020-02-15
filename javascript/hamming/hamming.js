export const compute = (leftStrand, rightStrand) => {
  if (leftStrand === rightStrand) {
    return 0
  }
  validiateStrandLengths(leftStrand, rightStrand)

  return (
    leftStrand
    .split("")
    .filter(isDifferent(rightStrand))
    .length
  )
}

function validiateStrandLengths(leftStrand, rightStrand) {
  if (leftStrand === "") {
    throw new Error("left strand must not be empty")
  }

  if (rightStrand === "") {
    throw new Error("right strand must not be empty")
  }

  if (leftStrand.length !== rightStrand.length) {
    throw new Error("left and right strands must be of equal length")
  }
}

function isDifferent(rightStrand) {
  return (leftNucleotide, index) => leftNucleotide !== rightStrand.charAt(index)
}
