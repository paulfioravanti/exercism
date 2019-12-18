export const isPangram = phrase => {
  return letterCount(phrase) === NUMBER_OF_LETTERS_IN_ALPHABET
}

function letterCount(phrase) {
  const letters = phrase
    .toLowerCase()
    .replace(NON_ASCII_LETTERS, "")
    .split("")
  return [...new Set(letters)].length
}

const NON_ASCII_LETTERS = /[^a-z]/g
const NUMBER_OF_LETTERS_IN_ALPHABET = 26
