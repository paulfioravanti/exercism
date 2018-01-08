# frozen_string_literal: true

module PigLatin
  SUFFIX = "ay"
  private_constant :SUFFIX
  VOWELS = "aeiou"
  private_constant :VOWELS
  VOWEL_SOUND = [*VOWELS.chars, "xr", /\Ay[^#{VOWELS}]/].freeze
  private_constant :VOWEL_SOUND
  CONSONANT_SOUND =
    /\A(?<consonant_sound>(?:.qu|qu|.)[^#{VOWELS}y]*)(?<rest>.*)/
  private_constant :CONSONANT_SOUND

  module_function

  def translate(phrase)
    phrase.split.map(&method(:translate_word)).join(" ")
  end

  def translate_word(word)
    if word.start_with?(*VOWEL_SOUND)
      word + SUFFIX
    else
      word.match(CONSONANT_SOUND) do |match|
        match[:rest] + match[:consonant_sound] + SUFFIX
      end
    end
  end
  private_class_method :translate_word
end

module BookKeeping
  VERSION = 2
end
