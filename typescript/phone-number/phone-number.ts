type Maybe<T> = T | undefined
type MaybeNull<T> = T | null

export default class PhoneNumber {
  private readonly VALID_NUMBER: RegExp = new RegExp([
    "^",
    "\\+?1?",                       // optional country code
    "\\s*",                         // optional divider
    "\\(?",                         // optional left parenthesis
    "(?<areaCode>[2-9]\\d{2})",     // area code
    "\\)?",                         // optional right parenthesis
    "[-\\.\\s]*",                   // optional divider
    "(?<exchangeCode>[2-9]\\d{2})", // exchange code
    "[-\\.\\s]*",                   // optional divider
    "(?<subscriberNumber>\\d{4})",  // subscriber number
    "\\s*",
    "$"
  ].join(""))
  private readonly _number: Maybe<string>

  constructor(input: string) {
    const maybeNumber: MaybeNull<RegExpExecArray> =
      this.VALID_NUMBER.exec(input)

    if (maybeNumber) {
      this._number = maybeNumber.slice(1, 4).join("")
    } else {
      this._number = undefined
    }
  }

  number(): Maybe<string> {
    return this._number
  }
}
