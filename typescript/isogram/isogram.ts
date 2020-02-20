export default abstract class Isogram {
  private static readonly WORD_CHARACTERS: RegExp = /\w/g

  static isIsogram(input: string): boolean {
    const isogramLetters: string[] = Isogram.toWordArray(input)
    return Isogram.uniq(isogramLetters).length === isogramLetters.length
  }

  private static toWordArray(input: string): string[] {
    return input.toLowerCase().match(Isogram.WORD_CHARACTERS) || []
  }

  private static uniq(array: string[]): string[] {
    return [...new Set(array)]
  }
}
