const GREATER = 1
const EQUAL = 0
const IS_NON_NATURAL_NUMBER = number => number < 1
const IS_FACTOR = (candidateFactor, number) => number % candidateFactor === 0

export const classify = number => {
  if (IS_NON_NATURAL_NUMBER(number)) {
    throw new Error("Classification is only possible for natural numbers.")
  }

  const characterisation = Math.sign(aliquotSum(number) - number)
  switch (characterisation) {
    case GREATER:
      return "abundant"
    case EQUAL:
      return "perfect"
    default:
      return "deficient"
  }
}

function aliquotSum(number) {
  return (
    range(number)
      .reduce(addFactor(number), [])
      .reduce(add, 0)
  )
}

function range(number) {
  return [...Array(number + 1).keys()].slice(1, number)
}

function addFactor(number) {
  return (acc, candidateFactor) => {
    if (IS_FACTOR(candidateFactor, number)) {
      acc.push(candidateFactor)
    }
    return acc
  }
}

function add(acc, factor) {
  return acc + factor
}
