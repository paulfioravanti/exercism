export default class Pangram {
  private readonly NON_ASCII_LETTERS: RegExp = /[^a-z]/g
  private readonly NUMBER_OF_LETTERS_IN_ALPHABET: Readonly<number> = 26
  private readonly phrase: Readonly<string>

  constructor(phrase: string) {
    this.phrase = phrase
  }

  isPangram(): boolean {
    return this.letterCount() === this.NUMBER_OF_LETTERS_IN_ALPHABET
  }

  private letterCount(): number {
    const letters: string[] =
      this.phrase
        .toLowerCase()
        .replace(this.NON_ASCII_LETTERS, "")
        .split("")

    return this.uniq(letters).length
  }

  private uniq(letters: string[]): string[] {
    return [...new Set(letters)]
  }
}
