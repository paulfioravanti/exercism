struct Pangram {
  private static let NUMBER_OF_LETTERS_IN_ALPHABET = 26

  static func isPangram(_ sentence: String) -> Bool {
    let uniqueLetters: Set<Character> =
      Set(sentence.lowercased().filter(isAsciiLetter))
    return uniqueLetters.count == NUMBER_OF_LETTERS_IN_ALPHABET
  }

  private static func isAsciiLetter(_ character: Character) -> Bool {
    return character.isLetter && character.isASCII
  }
}
