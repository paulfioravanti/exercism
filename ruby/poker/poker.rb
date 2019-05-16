# frozen_string_literal: true

require "pry"
require "bigdecimal"
class Poker
  CARD_VALUE = ->(num) { BigDecimal(num) * BigDecimal("0.01") }
  private_constant :CARD_VALUE
  RANKS = %w[a 2 3 4 5 6 7 8 9 10 J Q K A].freeze
  private_constant :RANKS

  class Hand
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
        # Assign a value to a card of less than 1 dependent on its rank
        # so that:
        # - cards can be compared to each other
        # - the value can be tallied along with hand scores
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

    module Score
      ACES = ->(card) { card.rank == "A" }
      private_constant :ACES
      POINTS = {
        one_pair: 1,
        two_pair: 2,
        three_of_a_kind: 3,
        straight: 5
      }.freeze
      private_constant :POINTS
      SCORE = /score\z/.freeze
      private_constant :SCORE
      SINGLE_CARD_GAP = CARD_VALUE.call(1)
      private_constant :SINGLE_CARD_GAP

      module_function

      def calculate(hand)
        private_instance_methods
          .select { |method| method.match?(SCORE) }
          .sum { |method| send(method, hand) }
      end

      def straight_score(hand)
        cards = hand.cards.sort
        straight = straight?(cards) || ace_low_straight?(cards)

        if straight
          POINTS[:straight] + cards.last.value
        else
          0
        end
      end
      private_class_method :straight_score

      def three_of_a_kind_score(hand)
        three_of_a_kind_card =
          catch(:halt) { find_multicard(hand, method(:determine_threes)) }
          .first

        if three_of_a_kind_card
          POINTS[:three_of_a_kind] + three_of_a_kind_card.value
        else
          0
        end
      end
      private_class_method :three_of_a_kind_score

      def two_pair_score(hand)
        pair_cards = find_multicard(hand, method(:determine_pair))
        if pair_cards.length > 1
          POINTS[:two_pair] + pair_cards.max.value
        else
          0
        end
      end
      private_class_method :two_pair_score

      def one_pair_score(hand)
        max_pair_card = find_multicard(hand, method(:determine_pair)).max
        max_pair_card ? POINTS[:one_pair] + max_pair_card.value : 0
      end
      private_class_method :one_pair_score

      def high_card_score(hand)
        hand.cards.max.value
      end
      private_class_method :high_card_score

      def find_multicard(hand, method)
        hand
          .cards
          .each
          .with_object(hand.ranks)
          .each_with_object([], &method)
          .uniq
      end
      private_class_method :find_multicard

      def determine_pair((card, ranks), acc)
        acc << card if ranks.count(card.rank) > 1
      end
      private_class_method :determine_pair

      def determine_threes((card, ranks), _acc)
        throw(:halt, [card]) if ranks.count(card.rank) > 2
      end
      private_class_method :determine_threes

      def straight?(cards)
        cards
          .each_cons(2)
          .all? { |(card1, card2)| card2 - card1 == SINGLE_CARD_GAP }
      end
      private_class_method :straight?

      def ace_low_straight?(cards)
        aces, other_cards = cards.partition(&ACES)
        return false unless aces.length == 1

        ace_low = Card.new("a#{aces.first.suit}")
        straight?(other_cards.prepend(ace_low))
      end
    end
    private_constant :Score

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
    initial_best_hands =
      { best_hands: [first_hand.to_a], best_hand: first_hand }
    rest
      .each_with_object(initial_best_hands, &method(:compare_best_hand))
      .fetch(:best_hands)
  end

  private

  attr_reader :hands

  def compare_best_hand(hand, acc)
    best_hand = acc[:best_hand]
    if hand == best_hand
      acc[:best_hands] << hand
    elsif hand > best_hand
      acc[:best_hands] = [hand.to_a]
      acc[:best_hand] = hand
    else
      acc
    end
  end
end
