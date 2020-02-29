export class RotationalCipher {
  static get ALPHABET_LENGTH() {
    return 26
  }

  static get LOWERCASE() {
    return /[a-z]/
  }

  static get UPPERCASE() {
    return /[A-Z]/
  }

  static rotate(text, shift) {
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

  static rotateChar(shift) {
    return (char) => {
      if (RotationalCipher.LOWERCASE.test(char)) {
        return RotationalCipher.rotateWithBase("a", char, shift)
      }

      if (RotationalCipher.UPPERCASE.test(char)) {
        return RotationalCipher.rotateWithBase("A", char, shift)
      }

      return char
    }
  }

  static rotateWithBase(base, char, shift) {
    const baseCodePoint = base.codePointAt(0)
    const rotatedCodePoint =
      (char.codePointAt(0) - baseCodePoint + shift)
        % RotationalCipher.ALPHABET_LENGTH
        + baseCodePoint

    return String.fromCodePoint(rotatedCodePoint)
  }
}
