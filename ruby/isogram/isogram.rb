# frozen_string_literal: true

module Isogram
  WORD_CHARACTERS = /\w/.freeze
  private_constant :WORD_CHARACTERS

  module_function

  def isogram?(input)
    isogram_letters = input.downcase.scan(WORD_CHARACTERS)
    isogram_letters.uniq == isogram_letters
  end
end
