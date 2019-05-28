# frozen_string_literal: true

module Brackets
  OPENING_BRACKETS = "([{"
  private_constant :OPENING_BRACKETS
  CLOSING_BRACKETS = ")]}"
  private_constant :CLOSING_BRACKETS
  WHITESPACE = /\s/.freeze
  private_constant :WHITESPACE

  module_function

  def paired?(string)
    string = remove_white_space(string)
    return true if string.empty?

    catch(:halt) do
      string
        .each_char
        .with_object([], &method(:check_pair))
        .empty?
    end
  end

  def remove_white_space(string)
    string.gsub(WHITESPACE, "")
  end
  private_class_method :remove_white_space

  def check_pair(char, acc)
    throw(:halt, false) if
      closing_bracket?(char) && no_matching_opening_bracket?(acc, char)

    acc.push(char) if opening_bracket?(char)
  end
  private_class_method :check_pair

  def closing_bracket?(char)
    CLOSING_BRACKETS.include?(char)
  end
  private_class_method :closing_bracket?

  def no_matching_opening_bracket?(acc, char)
    acc.pop != char.tr(CLOSING_BRACKETS, OPENING_BRACKETS)
  end

  def opening_bracket?(char)
    OPENING_BRACKETS.include?(char)
  end
  private_class_method :opening_bracket?
end
