# frozen_string_literal: true

class WordProblem
  MATH_QUESTION =
    /\A
    What\sis\s                           # What is
    (?<initial_value>-?\d+)\s            # 1, -1 etc
    (?<messages>(\w+(\sby)?\s-?\d+\s?)+) # plus 1, multiplied by 1 plus 2 etc
    \?                                   # question mark at the end
    \z/x
  private_constant :MATH_QUESTION
  MESSAGES = {
    "plus" => :+,
    "minus" => :-,
    "multiplied" => :*,
    "divided" => :/
  }.freeze
  private_constant :MESSAGES

  attr_reader :initial_value, :messages

  def initialize(question)
    raise ArgumentError unless (match = question.match(MATH_QUESTION))
    @initial_value = match[:initial_value].to_i
    @messages = parse_messages(match[:messages])
  end

  def answer
    messages.reduce(initial_value) do |acc, (message, value)|
      acc.public_send(message, value)
    end
  end

  private

  def parse_messages(messages)
    messages
      .gsub("by ", "")
      .split
      .each_slice(2)
      .map { |message, value| [MESSAGES[message], value.to_i] }
  end
end

module BookKeeping
  VERSION = 1
end
