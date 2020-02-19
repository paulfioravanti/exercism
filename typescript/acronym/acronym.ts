export default class Acronym {
  private static readonly NON_ACRONYM_TARGET: RegExp = /(\w)[A-Z]*[a-z]*\W*/g

  public static parse(phrase: string): string {
    return phrase.replace(Acronym.NON_ACRONYM_TARGET, "$1").toUpperCase()
  }
}
