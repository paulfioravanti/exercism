# frozen_string_literal: true

module RotationalCipher
  ALPHABET_LENGTH = 26
  private_constant :ALPHABET_LENGTH
  ALPHA_CHARACTER = /[A-Za-z]/.freeze
  private_constant :ALPHA_CHARACTER
  DEFAULT_BASE = 0
  private_constant :DEFAULT_BASE
  INITIAL_CIPHER = ""
  private_constant :INITIAL_CIPHER
  IS_LOWERCASE = ->(char) { char.between?("a", "z") }
  private_constant :IS_LOWERCASE
  IS_UPPERCASE = ->(char) { char.between?("A", "Z") }
  private_constant :IS_UPPERCASE
  LOWER_BASE = "a".ord
  private_constant :LOWER_BASE
  UPPER_BASE = "A".ord
  private_constant :UPPER_BASE

  module_function

  def rotate(string, key)
    string.chars.reduce(INITIAL_CIPHER) do |acc, char|
      next (acc + char) unless char.match?(ALPHA_CHARACTER)

      acc + rotate_character(char, key)
    end
  end

  def rotate_character(char, key)
    base = base_for(char)
    char
      .ord
      .then { |num| num - base + key }
      .modulo(ALPHABET_LENGTH)
      .then { |num| num + base }
      .chr
  end
  private_class_method :rotate_character

  def base_for(char)
    case char
    when IS_LOWERCASE
      LOWER_BASE
    when IS_UPPERCASE
      UPPER_BASE
    else
      DEFAULT_BASE
    end
  end
  private_class_method :base_for
end
