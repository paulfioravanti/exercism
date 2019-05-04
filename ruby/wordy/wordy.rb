# frozen_string_literal: true

class WordProblem
  MATH_QUESTION =
    /\A
    What\sis\s                           # What is
    (?<initial_value>-?\d+)\s            # 1, -1 etc
    (?<messages>(\w+(\sby)?\s-?\d+\s?)+) # plus 1, multiplied by 1 plus 2 etc
    \?                                   # question mark at the end
    \z/x.freeze
  private_constant :MATH_QUESTION
  MESSAGES = {
    "plus" => :+,
    "minus" => :-,
    "multiplied" => :*,
    "divided" => :/
  }.freeze
  private_constant :MESSAGES

  def initialize(question)
    @question = question
  end

  def answer
    raise ArgumentError unless (match = question.match(MATH_QUESTION))

    initial_value = match[:initial_value].to_i
    messages = parse_messages(match[:messages])
    messages.reduce(initial_value) do |acc, (message, value)|
      acc.public_send(message, value)
    end
  end

  private

  attr_reader :question

  def parse_messages(messages)
    messages
      .gsub("by ", "")
      .split
      .each_slice(2)
      .map { |message, value| [MESSAGES[message], value.to_i] }
  end
end
