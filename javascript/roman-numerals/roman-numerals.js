const ROMAN_NUMERALS = Object.freeze([
  ["M", 1000],
  ["CM", 900],
  ["D", 500],
  ["CD", 400],
  ["C", 100],
  ["XC", 90],
  ["L", 50],
  ["XL", 40],
  ["X", 10],
  ["IX", 9],
  ["V", 5],
  ["IV", 4],
  ["I", 1]
])

export const toRoman = num => {
  return ROMAN_NUMERALS.reduce(appendRomanNumeral, ["", num])[0]
}

function appendRomanNumeral([numeral, number], [roman, decimal]) {
  const quotient = Math.floor(number / decimal)
  const remainder = number % decimal
  const str = numeral + roman.repeat(quotient)

  return [str, remainder]
}
