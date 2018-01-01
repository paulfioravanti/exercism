# frozen_string_literal: true

module FoodChain
  ANIMALS = %w[fly spider bird cat dog goat cow horse].freeze
  private_constant :ANIMALS
  WRIGGLED_AND_JIGGLED_AND_TICKLED_INSIDE_HER =
    "wriggled and jiggled and tickled inside her."
  private_constant :WRIGGLED_AND_JIGGLED_AND_TICKLED_INSIDE_HER
  COMMENTARY_ON_SWALLOWING_ANIMAL = Hash[
    ANIMALS.zip(
      [
        nil,
        "It #{WRIGGLED_AND_JIGGLED_AND_TICKLED_INSIDE_HER}",
        "How absurd to swallow a bird!",
        "Imagine that, to swallow a cat!",
        "What a hog, to swallow a dog!",
        "Just opened her throat and swallowed a goat!",
        "I don't know how she swallowed a cow!",
        nil
      ]
    )
  ].freeze
  private_constant :COMMENTARY_ON_SWALLOWING_ANIMAL
  FOLLOW_UP_FOR_SWALLOWING_ANIMAL = lambda do |animal|
    if animal == "horse"
      "She's dead, of course!"
    else
      FOLLOW_UP_ACTION_STACK[STACK_HEIGHT[animal]..-1].join("\n")
    end
  end
  private_constant :FOLLOW_UP_FOR_SWALLOWING_ANIMAL
  VERSES_FOR = lambda do |animal|
    [
      "I know an old lady who swallowed a #{animal}.",
      COMMENTARY_ON_SWALLOWING_ANIMAL[animal],
      FOLLOW_UP_FOR_SWALLOWING_ANIMAL[animal]
    ].compact.join("\n")
  end
  private_constant :VERSES_FOR
  SHE_SWALLOWED_THE = lambda do |animal, phrase|
    "She swallowed the #{animal} #{phrase}"
  end
  private_constant :SHE_SWALLOWED_THE
  TO_CATCH_THE = lambda do |animal|
    to_catch = "to catch the #{animal}"
    if animal == "spider"
      "#{to_catch} that #{WRIGGLED_AND_JIGGLED_AND_TICKLED_INSIDE_HER}"
    else
      "#{to_catch}."
    end
  end
  private_constant :TO_CATCH_THE
  STACK_HEIGHT = Hash[ANIMALS[0..-2].reverse.zip((-7..-1))].freeze
  private_constant :STACK_HEIGHT
  FOLLOW_UP_ACTION_STACK = [].tap do |array|
    ANIMALS[0..-2].reverse.each_cons(2) do |animal, other_animal|
      array << SHE_SWALLOWED_THE[animal, TO_CATCH_THE[other_animal]]
    end
    array << "I don't know why she swallowed the fly. Perhaps she'll die.\n"
  end.freeze
  private_constant :FOLLOW_UP_ACTION_STACK

  module_function

  def song
    ANIMALS.map { |animal| VERSES_FOR[animal] }.join("\n") + "\n"
  end
end

module BookKeeping
  VERSION = 2
end
