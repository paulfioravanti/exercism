export const findAnagrams = (word, candidates) => {
  return candidates.filter(isAnagram(word.toLowerCase()))
}

function isAnagram(word) {
  return (anagramCandidate) => {
    const candidate = anagramCandidate.toLowerCase()
    return (
      word !== candidate &&
      toSortedString(word) === toSortedString(candidate)
    )
  }
}

function toSortedString(word) {
  return word.split("").sort().join()
}
