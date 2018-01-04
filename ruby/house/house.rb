# frozen_string_literal: true

module House
  OBJECTS = [
    "the house that Jack built.",
    "the malt",
    "the rat",
    "the cat",
    "the dog",
    "the cow with the crumpled horn",
    "the maiden all forlorn",
    "the man all tattered and torn",
    "the priest all shaven and shorn",
    "the rooster that crowed in the morn",
    "the farmer sowing his corn",
    "the horse and the hound and the horn"
  ].freeze
  # private_constant :OBJECTS
  VERBS = [
    nil,
    "lay in",
    "ate",
    "killed",
    "worried",
    "tossed",
    "milked",
    "kissed",
    "married",
    "woke",
    "kept",
    "belonged to"
  ].freeze
  DESCRIPTORS = VERBS.zip(OBJECTS)
  RHYME_FOR = lambda do |object, index|
    ["This is #{object}"].tap do |arr|
      descriptors = DESCRIPTORS[0..index].reverse
      descriptors.each.with_index do |(verb, _obj), idx|
        next if verb.nil?
        arr << "that #{verb} #{descriptors[idx + 1][1]}"
      end
    end.join("\n") + "\n"
  end
  private_constant :RHYME_FOR

  module_function

  def recite
    OBJECTS.map.with_index do |object, index|
      RHYME_FOR[object, index]
    end.join("\n")
  end
end
