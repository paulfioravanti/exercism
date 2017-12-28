# frozen_string_literal: true

class Say
  NUMBERS = %w[
    zero one two three four five six seven eight nine ten eleven twelve
    thirteen fourteen fifteen sixteen seventeen eighteen nineteen
  ].freeze
  private_constant :NUMBERS
  TENS = %w[
    nil nil twenty thirty forty fifty sixty seventy eighty ninety
  ].freeze
  private_constant :TENS
  NUMBER_COMPOUNDS = lambda do |number|
    case number.to_s.size
    when 2
      [10, TENS[number / 10]]
    when 3
      [100, "hundred"]
    when 4..6
      [1000, "thousand"]
    when 7..9
      [1_000_000, "million"]
    when 10..12
      [1_000_000_000, "billion"]
    end
  end
  private_constant :NUMBER_COMPOUNDS
  MAX_RANGE = 0..999_999_999_999
  private_constant :MAX_RANGE

  def initialize(number)
    @number = number
  end

  def in_english
    raise ArgumentError unless MAX_RANGE.cover?(number)
    number_to_english(number)
  end

  private

  attr_reader :number

  def number_to_english(number)
    if number < 20
      NUMBERS[number]
    elsif number < 100
      build_hyphened_word(number)
    else
      build_full_word(number)
    end
  end

  def build_hyphened_word(number)
    build_word(number) do |word, _divisor|
      [word, ->(num) { "-#{NUMBERS[num]}" }]
    end
  end

  def build_full_word(number)
    build_word(number) do |word, divisor|
      [
        "#{number_to_english(number / divisor)} #{word}",
        ->(num) { " #{number_to_english(num)}" }
      ]
    end
  end

  def build_word(number)
    divisor, word = NUMBER_COMPOUNDS.call(number)
    head, remainder_function = yield(word, divisor)
    tail = (num = number % divisor).nonzero? ? remainder_function.call(num) : ""
    "#{head}#{tail}"
  end
end

module BookKeeping
  VERSION = 1
end
