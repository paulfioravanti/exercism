# frozen_string_literal: true

module BeerSong
  AMOUNT_OF_BEER = "%<amount>s %<container>s of beer"
  private_constant :AMOUNT_OF_BEER
  BEER_ON_WALL = lambda do |amount, container: "bottles"|
    "#{format(AMOUNT_OF_BEER, amount: amount, container: container)} "\
    "on the wall"
  end
  private_constant :BEER_ON_WALL
  END_VERSE_NUMBER = lambda do |start_verse_number, num_verses|
    start_verse_number - num_verses + 1
  end
  private_constant :END_VERSE_NUMBER
  LINE_1 = lambda do |amount, container: "bottles"|
    "#{BEER_ON_WALL.call(amount.to_s.capitalize, container: container)}, "\
    "#{format(AMOUNT_OF_BEER, amount: amount, container: container)}."
  end
  private_constant :LINE_1
  LINE_2 = lambda do |amount, container: "bottles", subject: "one"|
    "Take #{subject} down and pass it around, "\
    "#{BEER_ON_WALL.call(amount, container: container)}."
  end
  private_constant :LINE_2
  GENERIC_VERSE = lambda do |amount|
    <<~VERSE
      #{LINE_1.call(amount)}
      #{LINE_2.call(amount - 1)}
    VERSE
  end
  private_constant :GENERIC_VERSE
  VERSE_2 =
    <<~VERSE
      #{LINE_1.call(2)}
      #{LINE_2.call(1, container: "bottle")}
    VERSE
  private_constant :VERSE_2
  VERSE_1 =
    <<~VERSE
      #{LINE_1.call(1, container: "bottle")}
      #{LINE_2.call("no more", subject: "it")}
    VERSE
  private_constant :VERSE_1
  VERSE_0 =
    <<~VERSE
      #{LINE_1.call("no more")}
      Go to the store and buy some more, #{BEER_ON_WALL.call(99)}.
    VERSE
  private_constant :VERSE_0
  VERSES =
    Hash.new { |_hash, key| GENERIC_VERSE.call(key) }.tap do |hash|
      hash[2] = VERSE_2
      hash[1] = VERSE_1
      hash[0] = VERSE_0
    end
  private_constant :VERSES

  module_function

  def recite(start_verse_number, num_verses)
    end_verse_number =
      END_VERSE_NUMBER.call(start_verse_number, num_verses)

    start_verse_number
      .downto(end_verse_number)
      .map(&VERSES)
      .join("\n")
  end
end
