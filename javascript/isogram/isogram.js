const WORD_CHARACTERS = /\w/g

export const isIsogram = input => {
  if (input === '') {
    return true
  }

  const isogramLetters = input.toLowerCase().match(WORD_CHARACTERS)
  return isEqual(uniq(isogramLetters), isogramLetters)
};

function uniq(array) {
  return [...new Set(array)]
}

function isEqual(array1, array2) {
  return (
    array1.length === array2.length &&
    array1.every(isContainedIn(array2))
  )
}

function isContainedIn(array) {
  return (char, index) => {
    return array[index] === char
  }
}
