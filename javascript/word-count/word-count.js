const WORD = /\b[\w']+\b/g

export const countWords = sentence => {
  const words = convertToWords(sentence)
  return words.reduce(incrementTallyForWord, {})
}

function convertToWords(sentence) {
  return sentence.toLowerCase().match(WORD)
}

function incrementTallyForWord(acc, word) {
  acc[word] ? acc[word] += 1 : acc[word] = 1
  return acc
}
