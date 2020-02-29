export class RotationalCipher {
  static get ALPHABET_LENGTH() {
    return 26
  }

  static rotate(text, shift) {
    if (shift === 0) {
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
      const lower = char.toLowerCase()
      const upper = char.toUpperCase()

      if (char === lower && char !== upper) {
        return RotationalCipher.rotateWithBase("a", char, shift)
      }

      if (char === upper && char !== lower) {
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
