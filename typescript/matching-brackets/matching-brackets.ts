type OpeningBracket = "(" | "[" | "{"
type ClosingBracket = ")" | "]" | "}"
type Brackets = Readonly<Record<OpeningBracket, ClosingBracket>>
type OpeningBracketStack = Array<OpeningBracket>;
type Maybe<T> = T | undefined
type MaybeError<T> = T | never

export default class MatchingBrackets {
  private readonly BRACKETS: Brackets = Object.freeze({
    "(": ")",
    "[": "]",
    "{": "}"
  })
  private readonly input: Readonly<string>

  constructor(input: string) {
    this.input = input
  }

  isPaired(): boolean {
    try {
      const unpaired: OpeningBracketStack =
        this.input
          .split("")
          .reduce(this.checkPair.bind(this), [])

      return unpaired.length === 0
    } catch(_error) {
      return false
    }
  }

  private checkPair(
    acc: OpeningBracketStack,
    character: string
  ): OpeningBracketStack {
    if (this.isValidOpeningBracket(character)) {
      acc.push(character as OpeningBracket)
      return acc
    }

    return this.handlePotentialClosingBracket(acc, character)
  }

  private isValidOpeningBracket(character: string): boolean {
    return Object.keys(this.BRACKETS).includes(character as OpeningBracket)
  }

  private handlePotentialClosingBracket(
    acc: OpeningBracketStack,
    character: string
  ): MaybeError<OpeningBracketStack> {
    if (this.isValidClosingBracket(character)) {
      const openingBracketCandidate: Maybe<OpeningBracket> = acc.pop()

      if (openingBracketCandidate === undefined) {
        return acc
      }

      const closingBracket: ClosingBracket = character as ClosingBracket

      if (!this.arePaired(openingBracketCandidate, closingBracket)) {
        throw "halt"
      }
    }

    return acc
  }

  private isValidClosingBracket(character: string): boolean {
    return Object.values(this.BRACKETS).includes(character as ClosingBracket)
  }

  private arePaired(
    openingBracketCandidate: OpeningBracket,
    closingBracket: ClosingBracket
  ): boolean {
    const matchingOpeningBracket: OpeningBracket =
      Object
        .keys(this.BRACKETS)
        .find(this.keyForValue(closingBracket)) as OpeningBracket

    return openingBracketCandidate === matchingOpeningBracket
  }

  private keyForValue(closingBracket: ClosingBracket) {
    return (key: string): boolean => {
      return this.BRACKETS[key as OpeningBracket] === closingBracket
    }
  }
}
