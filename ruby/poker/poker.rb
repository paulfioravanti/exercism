# frozen_string_literal: true

require "pry"
require "bigdecimal"
class Poker
  # Assign a value to a card of less than 1 dependent on its rank
  # so that:
  # - cards can be compared to each other
  # - the value can be tallied along with hand scores
  CARD_VALUE = ->(num) { BigDecimal(num) * BigDecimal("0.01") }
  private_constant :CARD_VALUE
  RANKS = %w[a 2 3 4 5 6 7 8 9 10 J Q K A].freeze
  private_constant :RANKS

  module Score
    module HighLowCard
      module_function

      def call(hand)
        min, max = hand.cards.minmax
        max.value + min.value / 2
      end
    end

    module OnePair
      POINTS = 1
      private_constant :POINTS

      module_function

      def call(hand)
        max_pair_card = Score.find_multicard(hand, 1).max
        max_pair_card ? POINTS + max_pair_card.value : 0
      end
    end

    module TwoPair
      POINTS = 2
      private_constant :POINTS

      module_function

      def call(hand)
        pair_cards = Score.find_multicard(hand, 1)
        pair_cards.length > 1 ? POINTS + pair_cards.max.value : 0
      end
    end

    module ThreeOfAKind
      POINTS = 3
      private_constant :POINTS

      module_function

      def call(hand)
        three_of_a_kind_card = Score.find_multicard(hand, 2).max
        three_of_a_kind_card ? POINTS + three_of_a_kind_card.value : 0
      end
    end

    module Straight
      ACES = ->(card) { card.rank == "A" }
      private_constant :ACES
      POINTS = 5
      private_constant :POINTS
      SINGLE_CARD_GAP = CARD_VALUE.call(1)
      private_constant :SINGLE_CARD_GAP

      module_function

      def call(hand)
        straight = straight(hand)
        straight ? POINTS + straight.last.value : 0
      end

      def straight(hand)
        cards = hand.cards.sort
        numbered_straight(cards) || ace_low_straight(cards)
      end

      def numbered_straight(cards)
        straight =
          cards
          .each_cons(2)
          .all? { |(card1, card2)| card2 - card1 == SINGLE_CARD_GAP }
        straight ? cards : nil
      end
      private_class_method :numbered_straight

      def ace_low_straight(cards)
        aces, other_cards = cards.partition(&ACES)
        return nil unless aces.length == 1

        ace_low = Card.new("a#{aces.first.suit}")
        numbered_straight(other_cards.prepend(ace_low))
      end
      private_class_method :ace_low_straight
    end

    module Flush
      POINTS = 8
      private_constant :POINTS

      module_function

      def call(hand)
        flush?(hand) ? POINTS + hand.cards.max.value : 0
      end

      def flush?(hand)
        hand.suits.uniq.one?
      end
    end

    module FullHouse
      POINTS = 13
      private_constant :POINTS

      module_function

      def call(hand)
        three_set, other_cards = split_hand(hand)
        return 0 if not_full_house?(three_set, other_cards, hand.ranks)

        throw(:halt, POINTS + three_set.first.value)
      end

      def split_hand(hand)
        hand
          .cards
          .sort
          .partition { |card| Score.multiple?(card, hand.ranks, 2) }
      end
      private_class_method :split_hand

      def not_full_house?(three_set, other_cards, ranks)
        three_set.empty? ||
          other_cards.none? { |card| Score.multiple?(card, ranks, 1) }
      end
    end

    module FourOfAKind
      POINTS = 21
      private_constant :POINTS

      module_function

      def call(hand)
        square_cards, _other_cards = split_hand(hand)
        if (square = square_cards.first)
          throw(:halt, POINTS + square.value)
        else
          0
        end
      end

      def split_hand(hand)
        hand
          .cards
          .partition { |card| Score.multiple?(card, hand.ranks, 3) }
      end
    end

    module StraightFlush
      POINTS = 34
      private_constant :POINTS

      module_function

      def call(hand)
        if (straight = Straight.straight(hand)) && Flush.flush?(hand)
          throw(:halt, POINTS + straight.last.value)
        else
          0
        end
      end
    end

    module_function

    def calculate(hand)
      catch(:halt) do
        constants.sum { |mod| const_get(mod).call(hand) }
      end
    end

    def find_multicard(hand, floor)
      hand
        .cards
        .each
        .with_object(hand.ranks)
        .with_object(floor)
        .each_with_object([], &method(:add_multicard))
        .uniq
    end

    def add_multicard(((card, ranks), floor), acc)
      acc << card if multiple?(card, ranks, floor)
    end

    def multiple?(card, ranks, floor)
      ranks.count(card.rank) > floor
    end
  end
  private_constant :Score

  class Card
    include Comparable

    # Lookahead to non-digit character in string
    SUIT = /(?=\D)/.freeze
    private_constant :SUIT

    attr_reader :rank, :suit

    def initialize(card)
      @rank, @suit = card.split(SUIT)
    end

    def to_s
      rank + suit
    end

    def value(rank = self.rank)
      RANKS.index(rank).then(&CARD_VALUE)
    end

    def -(other)
      value - value(other.rank)
    end

    private

    def hash
      value.hash
    end

    def eql?(other)
      value.eql?(value(other.rank))
    end

    def <=>(other)
      value <=> value(other.rank)
    end
  end
  private_constant :Card

  class Hand
    include Comparable

    attr_reader :cards

    def initialize(cards)
      @cards = cards.map { |card| Card.new(card) }
      @score = Score.calculate(self)
    end

    def to_a
      cards.map(&:to_s)
    end

    def ranks
      cards.map(&:rank)
    end

    def suits
      cards.map(&:suit)
    end

    protected

    attr_reader :score

    private

    def <=>(other)
      score <=> other.score
    end
  end
  private_constant :Hand

  def initialize(hands)
    @hands = hands.map { |hand| Hand.new(hand) }
  end

  def best_hand
    first_hand, *rest = hands
    initial_best_hands = initial_best_hands(first_hand)

    rest
      .each_with_object(initial_best_hands, &method(:compare_best_hand))
      .fetch(:best_hands)
  end

  private

  attr_reader :hands

  def initial_best_hands(first_hand)
    { best_hands: [first_hand.to_a], best_hand: first_hand }
  end

  def compare_best_hand(hand, acc)
    best_hand = acc[:best_hand]
    if hand == best_hand
      acc[:best_hands] << hand.to_a
    elsif hand > best_hand
      acc[:best_hands] = [hand.to_a]
      acc[:best_hand] = hand
    else
      acc
    end
  end
end
