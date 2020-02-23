type Maybe<T> = T | undefined
type MaybeError<T> = T | never

export default class MatchingBrackets {
  private readonly OPENING_BRACKETS: string = "([{"
  private readonly CLOSING_BRACKETS: string = ")]}"
  private readonly input: string

  constructor(input: string) {
    this.input = input
  }

  isPaired(): boolean {
    try {
      const unpaired: string[] =
        this.input
          .split("")
          .reduce(this.checkPair.bind(this), [])

      return unpaired.length === 0
    } catch(_error) {
      return false
    }
  }

  private checkPair(acc: string[], character: string): string[] {
    if (this.OPENING_BRACKETS.includes(character)) {
      acc.push(character)
      return acc
    }

    return this.handlePotentialClosingBracket(acc, character)
  }

  private handlePotentialClosingBracket(
    acc: string[],
    character: string
  ): MaybeError<string[]> {
    if (this.CLOSING_BRACKETS.includes(character)) {
      const openingBracketCandidate: Maybe<string> = acc.pop()

      if (openingBracketCandidate === undefined) {
        return acc
      }

      if (
        !this.hasMatchingOpeningBracket(
          openingBracketCandidate,
          character
        )
      ) {
        throw "halt"
      }
    }

    return acc
  }

  private hasMatchingOpeningBracket(
    openingBracketCandidate: string,
    character: string
  ): boolean {
    const closingBracketIndex: number =
      this.CLOSING_BRACKETS.indexOf(character)
    const matchingOpeningBracket: string =
      this.OPENING_BRACKETS[closingBracketIndex]

    return openingBracketCandidate === matchingOpeningBracket
  }
}
