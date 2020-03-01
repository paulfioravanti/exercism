// NOTE: \1 backreferences the single character match in parentheses.
// The parentheses only serve to provide a match for the backreference.
const CONSECUTIVE_DATA_ELEMENTS = /([\w\s])\1+/g
const RUN_LENGTH_ENCODING = /(\d+)(\D)/g

export const encode = input => {
  if (input === "") {
    return input
  }

  return input.replace(CONSECUTIVE_DATA_ELEMENTS, compress)
}

export const decode = input => {
  if (input === "") {
    return input
  }

  return input.replace(RUN_LENGTH_ENCODING, reconstruct)
}

function compress(match, character) {
  return `${match.length}${character}`
}

function reconstruct(_match, digit, character) {
  return character.repeat(digit)
}
