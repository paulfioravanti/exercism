# frozen_string_literal: true

class Scrabble
  SCORE = lambda do |letter|
    case letter
    when "a", "e", "i", "o", "u", "l", "n", "r", "s", "t"
      1
    when "d", "g"
      2
    when "b", "c", "m", "p"
      3
    when "f", "h", "v", "w", "y"
      4
    when "k"
      5
    when "j", "x"
      8
    when "q", "z"
      10
    else
      0
    end
  end
  private_constant :SCORE

  def self.score(word)
    new(word).score
  end

  def initialize(word)
    @word = word.to_s.strip.downcase
  end

  def score
    word.chars.sum(&SCORE)
  end

  private

  attr_reader :word
end
