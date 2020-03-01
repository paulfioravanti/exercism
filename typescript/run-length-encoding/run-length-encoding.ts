export default abstract class RunLengthEncoding {
  // NOTE: \1 backreferences the single character match in parentheses.
  // The parentheses only serve to provide a match for the backreference.
  private static readonly CONSECUTIVE_DATA_ELEMENTS: RegExp = /([\w\s])\1+/g
  private static readonly RUN_LENGTH_ENCODING: RegExp = /(\d+)(\D)/g

  static encode(input: string): string {
    if (input === "") {
      return input
    }

    return (
      input.replace(
        RunLengthEncoding.CONSECUTIVE_DATA_ELEMENTS,
        RunLengthEncoding.compress
      )
    )
  }

  static decode(input: string): string {
    if (input === "") {
      return input
    }

    return (
      input.replace(
        RunLengthEncoding.RUN_LENGTH_ENCODING,
        RunLengthEncoding.reconstruct
      )
    )
  }

  private static compress(match: string, character: string): string {
    return `${match.length}${character}`
  }

  private static reconstruct(
    _match: string,
    digit: string,
    character: string
  ): string {
    return character.repeat(Number(digit))
  }
}
