# frozen_string_literal: true

class BeerSong
  AMOUNT_OF_BEER = ->(amount, container) { "#{amount} #{container} of beer" }
  private_constant :AMOUNT_OF_BEER
  BEER_ON_WALL = lambda { |amount, container: "bottles"|
    "#{AMOUNT_OF_BEER.call(amount, container)} on the wall"
  }
  private_constant :BEER_ON_WALL
  LINE_1 = lambda { |amount, container: "bottles"|
    "#{BEER_ON_WALL.call(amount.to_s.capitalize, container: container)}, "\
    "#{AMOUNT_OF_BEER.call(amount, container)}."
  }
  private_constant :LINE_1
  LINE_2 = lambda { |amount, container: "bottles", subject: "one"|
    "Take #{subject} down and pass it around, "\
    "#{BEER_ON_WALL.call(amount, container: container)}."
  }
  private_constant :LINE_2
  GENERIC_VERSE = lambda { |amount|
    <<~VERSE
      #{LINE_1.call(amount)}
      #{LINE_2.call(amount - 1)}
    VERSE
  }
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
    Hash.new { |_hash, key| const_get("GENERIC_VERSE").call(key) }.tap do |hash|
      hash[2] = VERSE_2
      hash[1] = VERSE_1
      hash[0] = VERSE_0
    end
  private_constant :VERSES

  def verse(number)
    VERSES[number]
  end

  def verses(from, to)
    from.downto(to).map { |number| verse(number) }.join("\n")
  end
end

module BookKeeping
  VERSION = 3
end
