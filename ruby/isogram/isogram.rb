# frozen_string_literal: true

module Isogram
  SPACES_AND_DASHES = " -"
  private_constant :SPACES_AND_DASHES

  module_function

  def isogram?(input)
    isogram_letters =
      input
      .downcase
      .delete(SPACES_AND_DASHES)
      .chars
      .sort

    isogram_letters.uniq == isogram_letters
  end
end
