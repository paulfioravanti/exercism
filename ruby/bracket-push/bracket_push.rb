# frozen_string_literal: true

module Brackets
  PAIRS = {
    "(" => ")",
    "[" => "]",
    "{" => "}"
  }.freeze
  private_constant :PAIRS
  LEFT = PAIRS.keys.freeze
  private_constant :LEFT
  RIGHT = PAIRS.values.freeze
  private_constant :RIGHT
  WHITESPACE = /\s/
  private_constant :WHITESPACE

  module_function

  def paired?(string)
    return true if (string = string.gsub(WHITESPACE, "")).empty?
    string.each_char.with_object([]) do |char, acc|
      return false if RIGHT.include?(char) && PAIRS[acc.pop] != char
      acc.push(char) if LEFT.include?(char)
    end.empty?
  end
end

module BookKeeping
  VERSION = 4
end
