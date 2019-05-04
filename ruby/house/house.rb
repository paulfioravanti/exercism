# frozen_string_literal: true

module House
  PHRASES = [
    { action: nil, subject: "the house that Jack built." },
    { action: "lay in", subject: "the malt" },
    { action: "ate", subject: "the rat" },
    { action: "killed", subject: "the cat" },
    { action: "worried", subject: "the dog" },
    { action: "tossed", subject: "the cow with the crumpled horn" },
    { action: "milked", subject: "the maiden all forlorn" },
    { action: "kissed", subject: "the man all tattered and torn" },
    { action: "married", subject: "the priest all shaven and shorn" },
    { action: "woke", subject: "the rooster that crowed in the morn" },
    { action: "kept", subject: "the farmer sowing his corn" },
    { action: "belonged to", subject: "the horse and the hound and the horn" }
  ].freeze
  private_constant :PHRASES
  SUBJECT = ->(subject) { "This is #{subject}" }
  private_constant :SUBJECT
  DESCRIPTION = ->(action, phrase) { "that #{action} #{phrase}" }
  private_constant :DESCRIPTION

  module_function

  def recite
    PHRASES.map.with_index do |phrase, index|
      rhyme_for(phrase, index) + "\n"
    end.join("\n")
  end

  def rhyme_for(phrase, index)
    phrases = PHRASES[0..index].reverse

    [SUBJECT.call(phrase[:subject])].tap do |array|
      phrases.each.with_index do |hash, phrase_index|
        next unless (action = hash[:action])

        array << DESCRIPTION.call(action, phrases[phrase_index + 1][:subject])
      end
    end.join("\n")
  end
  private_class_method :rhyme_for
end
