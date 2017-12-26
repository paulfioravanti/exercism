# frozen_string_literal: true

module Acronym
  WORD_TAIL = /(?!\b\w)./
  private_constant :WORD_TAIL

  module_function

  def abbreviate(phrase)
    phrase.gsub(WORD_TAIL, "").upcase
  end
end

module BookKeeping
  VERSION = 4
end
