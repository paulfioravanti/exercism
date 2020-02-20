const WORD_CHARACTERS = /\w/g

export const isIsogram = input => {
  if (input === '') {
    return true
  }

  const isogramLetters = input.toLowerCase().match(WORD_CHARACTERS)
  return uniq(isogramLetters).length === isogramLetters.length
}

function uniq(array) {
  return [...new Set(array)]
}
