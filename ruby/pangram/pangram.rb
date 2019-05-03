module Pangram
  NON_ASCII_LETTERS = /[^a-z]/.freeze
  private_constant :NON_ASCII_LETTERS
  NUMBER_OF_LETTERS_IN_ALPHABET = 26
  private_constant :NUMBER_OF_LETTERS_IN_ALPHABET

  module_function

  def pangram?(phrase)
    letter_count(phrase) == NUMBER_OF_LETTERS_IN_ALPHABET
  end

  def letter_count(phrase)
    phrase.downcase.gsub(NON_ASCII_LETTERS, "").chars.uniq.count
  end
  private_class_method :letter_count
end
