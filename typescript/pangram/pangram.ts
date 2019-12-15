class Pangram {
  private readonly NON_ASCII_LETTERS: RegExp = /[^a-z]/g
  private readonly NUMBER_OF_LETTERS_IN_ALPHABET: number = 26

  constructor(phrase: string) {
    this.phrase = phrase
  }

  isPangram(): boolean {
    return this.letterCount() === this.NUMBER_OF_LETTERS_IN_ALPHABET
  }

  private phrase: string

  private letterCount(): number {
    const letters = this.phrase
      .toLowerCase()
      .replace(this.NON_ASCII_LETTERS, "")
      .split("")
    return [...new Set(letters)].length
  }
}

export default Pangram
