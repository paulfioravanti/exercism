#  frozen_string_literal: true

module TwelveDays
  CARDINALS =
    Hash.new { |hash, key| hash[key] = key.gsub(/th\z/, "") }.tap do |hash|
      hash["first"] = "a"
      hash["second"] = "two"
      hash["third"] = "three"
      hash["fifth"] = "five"
      hash["eighth"] = "eight"
      hash["ninth"] = "nine"
      hash["twelfth"] = "twelve"
    end
  private_constant :CARDINALS
  DECLARATION_OF_RECEIPT =
    "On the %<day>s day of Christmas my true love gave to me: "
  private_constant :DECLARATION_OF_RECEIPT
  # Empty song holds an array for the generated song verses, and an array for
  # the "extra gifts" that need to be added to each consecutive day.
  EMPTY_SONG = [[], []].freeze
  private_constant :EMPTY_SONG
  GIFTS = [
    " Partridge in a Pear Tree",
    " Turtle Doves",
    " French Hens",
    " Calling Birds",
    " Gold Rings",
    " Geese-a-Laying",
    " Swans-a-Swimming",
    " Maids-a-Milking",
    " Ladies Dancing",
    " Lords-a-Leaping",
    " Pipers Piping",
    " Drummers Drumming"
  ].freeze
  private_constant :GIFTS
  NEWLINE = "\n"
  private_constant :NEWLINE
  ORDINALS = %w[
    first
    second
    third
    fourth
    fifth
    sixth
    seventh
    eighth
    ninth
    tenth
    eleventh
    twelfth
  ].freeze
  private_constant :ORDINALS
  PERIOD = "."
  private_constant :PERIOD

  module_function

  def song
    ORDINALS
      .zip(GIFTS)
      .each_with_object(EMPTY_SONG.dup, &method(:add_verse_to_song))
      .first
      .join(NEWLINE)
  end

  def add_verse_to_song((day, gift), (song, extra_gifts))
    current_gift = CARDINALS[day] + gift
    todays_gifts = current_gift + extra_gifts.join
    add_current_gift_to_extra_gifts(current_gift, extra_gifts)
    song << verse(day, todays_gifts)
  end
  private_class_method :add_verse_to_song

  def add_current_gift_to_extra_gifts(gift_of_the_day, extra_gifts)
    if extra_gifts.empty?
      extra_gifts << ", and #{gift_of_the_day}"
    else
      extra_gifts.prepend(", #{gift_of_the_day}")
    end
  end
  private_class_method :add_current_gift_to_extra_gifts

  def verse(day, all_gifts)
    format(DECLARATION_OF_RECEIPT, day: day) + all_gifts + PERIOD + NEWLINE
  end
  private_class_method :verse
end
