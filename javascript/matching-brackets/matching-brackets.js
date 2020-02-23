const OPENING_BRACKETS = "([{"
const CLOSING_BRACKETS = ")]}"

export const isPaired = input => {
  try {
    const unpaired = input.split("").reduce(checkPair, [])
    return unpaired.length === 0
  } catch(_error) {
    return false
  }
}

function checkPair(acc, character) {
  if (OPENING_BRACKETS.includes(character)) {
    acc.push(character)
    return acc
  }

  if (CLOSING_BRACKETS.includes(character)) {
    const openingBracketCandidate = acc.pop()
    if (!hasMatchingOpeningBracket(openingBracketCandidate, character)) {
      throw "halt"
    }
  }

  return acc
}

function hasMatchingOpeningBracket(openingBracketCandidate, character) {
  const closingBracketIndex = CLOSING_BRACKETS.indexOf(character)
  const matchingOpeningBracket = OPENING_BRACKETS[closingBracketIndex]
  return openingBracketCandidate === matchingOpeningBracket
}
