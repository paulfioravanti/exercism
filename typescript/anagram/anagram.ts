type AnagramFilter = (anagramCandidate: string) => boolean

export default class Anagram {
  private readonly word: string

  constructor(word: string) {
    this.word = word.toLowerCase()
  }

  matches(...candidates: string[]): string[] {
    return candidates.filter(this.isAnagram())
  }

  private isAnagram(): AnagramFilter {
    const word = this.word
    return (anagramCandidate: string): boolean => {
      const candidate = anagramCandidate.toLowerCase()
      return (
        word !== candidate &&
        this.normalize(word) === this.normalize(candidate)
      )
    }
  }

  private normalize(word: string): string {
    return word.split("").sort().join()
  }
}
