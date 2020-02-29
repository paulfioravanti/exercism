type Maybe<T> = T | undefined
type Base = "a" | "A"

export default abstract class RotationalCipher {
  private static readonly ALPHABET_LENGTH: number = 26
  private static readonly LOWERCASE: RegExp = /[a-z]/
  private static readonly UPPERCASE: RegExp = /[A-Z]/

  static rotate(text: string, shift: number): string {
    if (shift === 0 || shift === RotationalCipher.ALPHABET_LENGTH) {
      return text
    }

    return (
      text
        .split("")
        .map(RotationalCipher.rotateChar(shift))
        .join("")
    )
  }

  private static rotateChar(shift: number): (character: string) => string {
    return (character: string): string => {
      if (RotationalCipher.LOWERCASE.test(character)) {
        return RotationalCipher.rotateWithBase("a", character, shift)
      }

      if (RotationalCipher.UPPERCASE.test(character)) {
        return RotationalCipher.rotateWithBase("A", character, shift)
      }

      return character
    }
  }

  private static rotateWithBase(
    base: Base,
    character: string,
    shift: number
  ): string {
    const baseCodePoint: Maybe<number> = base.codePointAt(0)
    const characterCodePoint: Maybe<number> = character.codePointAt(0)

    if (baseCodePoint === undefined || characterCodePoint === undefined) {
      return character
    }

    const rotatedCodePoint: number =
      (characterCodePoint - baseCodePoint + shift)
        % RotationalCipher.ALPHABET_LENGTH
        + baseCodePoint

    return String.fromCodePoint(rotatedCodePoint)
  }
}
