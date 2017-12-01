class Phrase
  INCREMENT = 1
  private_constant :INCREMENT
  WORD = /\b[\w']+\b/
  private_constant :WORD
  ZERO_DEFAULT_HASH = Hash.new(0).freeze
  private_constant :ZERO_DEFAULT_HASH

  def initialize(phrase)
    @words = convert_to_words(phrase)
  end

  def word_count
    words.each_with_object(ZERO_DEFAULT_HASH.dup) do |word, occurrences|
      occurrences[word] += INCREMENT
    end
  end

  private

  attr_reader :words

  def convert_to_words(phrase)
    phrase.downcase.scan(WORD)
  end
end

module BookKeeping
  VERSION = 1
end
