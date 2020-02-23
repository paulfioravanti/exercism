export default class Anagram {
  private readonly word: Readonly<string>

  constructor(word: string) {
    this.word = word.toLowerCase()
  }

  matches(...candidates: string[]): string[] {
    return candidates.filter(this.isAnagram.bind(this))
  }

  private isAnagram(anagramCandidate: string): boolean {
    const word: Readonly<string> = this.word
    const candidate = anagramCandidate.toLowerCase()

    return (
      word !== candidate &&
      this.normalize(word) === this.normalize(candidate)
    )
  }

  private normalize(word: string): string {
    return word.split("").sort().join()
  }
}
