type MaybeMatchArray = RegExpMatchArray | null

export default abstract class Isogram {
  private static readonly WORD_CHARACTERS: RegExp = /\w/g

  static isIsogram(input: string): boolean {
    if (input === "") {
      return true
    }

    const isogramLetters: MaybeMatchArray = Isogram.toWordArray(input)

    if (!isogramLetters) {
      return false
    }

    return Isogram.uniq(isogramLetters).length === isogramLetters.length
  }

  private static toWordArray(input: string): MaybeMatchArray {
    return input.toLowerCase().match(Isogram.WORD_CHARACTERS)
  }

  private static uniq(array: string[]): string[] {
    return [...new Set(array)]
  }
}
