# frozen_string_literal: true

class Card
  include Comparable

  RANKS = %w[2 3 4 5 6 7 8 9 10 J Q K A].freeze
  private_constant :RANKS
  NON_DIGIT = /(?=\D)/
  private_constant :NON_DIGIT

  attr_reader :suit, :value

  def initialize(card)
    @value, @suit = card.split(NON_DIGIT)
  end

  def to_s
    value + suit
  end

  private

  def <=>(other)
    RANKS.index(value) <=> RANKS.index(other.value)
  end
end

class Hand
  def initialize(cards)
    @cards = cards.map { |card| Card.new(card) }
  end

  def cards
    @cards.map(&:to_s)
  end

  def high_card
    cards.max
  end
end

class Poker
  def initialize(hands)
    @hands = hands.map { |hand| Hand.new(hand) }
  end

  def best_hand
    return first_hand if hands.length == 1
    high_card_hand
  end

  private

  attr_reader :hands

  def first_hand
    [hands.first.cards]
  end

  def high_card_hand
    [hands.max_by(&:high_card).cards]
  end
end

module BookKeeping
  VERSION = 2
end