# frozen_string_literal: true

class Say
  MAX_RANGE = (0..999_999_999_999).freeze
  private_constant :MAX_RANGE

  module Number
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

    module_function

    def in_english(number)
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
    private_class_method :build_hyphened_word

    def build_full_word(number)
      build_word(number) do |word, divisor|
        [
          "#{in_english(number / divisor)} #{word}",
          ->(num) { " #{in_english(num)}" }
        ]
      end
    end
    private_class_method :build_full_word

    def build_word(number)
      divisor, word = NUMBER_COMPOUNDS.call(number)
      head, remainder_function = yield(word, divisor)
      tail =
        if (num = number % divisor).nonzero?
          remainder_function.call(num)
        else
          ""
        end
      "#{head}#{tail}"
    end
    private_class_method :build_word
  end
  private_constant :Number

  def initialize(number)
    @number = number
  end

  def in_english
    raise ArgumentError unless MAX_RANGE.cover?(number)

    Number.in_english(number)
  end

  private

  attr_reader :number
end
