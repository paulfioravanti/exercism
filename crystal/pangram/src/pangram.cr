module Pangram
  extend self

  private NON_ASCII_LOWERCASE_LETTERS   = "^a-z"
  private NUMBER_OF_LETTERS_IN_ALPHABET = 26

  def pangram?(sentence : String) : Bool
    letter_count(sentence) == NUMBER_OF_LETTERS_IN_ALPHABET
  end

  private def letter_count(sentence : String) : Int
    sentence
      .downcase
      .delete(NON_ASCII_LOWERCASE_LETTERS)
      .chars
      .uniq
      .size
  end
end
