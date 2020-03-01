type Translation = [string, number]
type Translations = Array<Translation>

export default abstract class RomanNumerals {
  private static readonly ROMAN_NUMERALS: Readonly<Translations> =
    Object.freeze([
      ["M", 1000],
      ["CM", 900],
      ["D", 500],
      ["CD", 400],
      ["C", 100],
      ["XC", 90],
      ["L", 50],
      ["XL", 40],
      ["X", 10],
      ["IX", 9],
      ["V", 5],
      ["IV", 4],
      ["I", 1]
    ])

  static roman(num: number): string {
    return (
      RomanNumerals.ROMAN_NUMERALS.reduce(
        RomanNumerals.appendRomanNumeral,
        ["", num]
      )[0]
    )
  }

  private static appendRomanNumeral(
    [numeral, num]: Translation,
    [roman, decimal]: Translation
  ): Translation {
    const quotient: number = Math.floor(num / decimal)
    const remainder: number = num % decimal
    const str: string = numeral + roman.repeat(quotient)

    return [str, remainder]
  }
}
