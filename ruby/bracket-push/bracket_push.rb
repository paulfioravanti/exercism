# frozen_string_literal: true

module Brackets
  OPEN = "([{"
  private_constant :OPEN
  CLOSE = ")]}"
  private_constant :CLOSE
  WHITESPACE = /\s/.freeze
  private_constant :WHITESPACE

  module_function

  def paired?(string)
    return true if (string = string.gsub(WHITESPACE, "")).empty?

    string.each_char.with_object([]) do |char, acc|
      return false if CLOSE.include?(char) && acc.pop != char.tr(CLOSE, OPEN)

      acc.push(char) if OPEN.include?(char)
    end.empty?
  end
end
