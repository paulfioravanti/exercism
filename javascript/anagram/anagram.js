export const findAnagrams = (base, candidates) => {
  const word = base.toLowerCase()
  const letters = toSortedLetters(word)
  return candidates.filter(isAnagram(word, letters))
}

function toSortedLetters(word) {
  return word.split("").sort()
}

function isAnagram(word, letters) {
  return (wordCandidate) => {
    const candidate = wordCandidate.toLowerCase()
    return (
      isDifferentWord(word, candidate) && hasSameLetters(letters, candidate)
    )
  }
}

function isDifferentWord(word, candidate) {
  return word !== candidate
}

function hasSameLetters(letters, candidate) {
  const candidateLetters = toSortedLetters(candidate)
  return (
    hasSameLength(letters, candidateLetters) &&
    includesEveryLetterOnce(letters, candidateLetters)
  )
}

function hasSameLength(letters, candidateLetters) {
  return letters.length === candidateLetters.length
}

function includesEveryLetterOnce(letters, candidateLetters) {
  return letters.every((letter, index) => candidateLetters[index] === letter)
}
