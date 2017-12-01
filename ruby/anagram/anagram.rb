class Anagram
  def initialize(word)
    @word = word.downcase
    @letters = @word.chars.sort
  end

  def match(candidates)
    candidates.select do |candidate|
      candidate = candidate.downcase
      word != candidate && letters == candidate.chars.sort
    end
  end

  private

  attr_reader :word, :letters
end

module BookKeeping
  VERSION = 2
end
