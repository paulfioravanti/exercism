export const findAnagrams = (word, candidates) => {
  return candidates.filter(isAnagram(word.toLowerCase()))
}

function isAnagram(word) {
  return (anagramCandidate) => {
    const candidate = anagramCandidate.toLowerCase()
    return word !== candidate && normalize(word) === normalize(candidate)
  }
}

function normalize(word) {
  return word.split("").sort().join()
}
