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
        straight: 5,
        flush: 8,
        full_house: 13,
        square: 21,
        straight_flush: 34
      }.freeze
      private_constant :POINTS
      SCORE = /score\z/.freeze
      private_constant :SCORE
      SINGLE_CARD_GAP = CARD_VALUE.call(1)
      private_constant :SINGLE_CARD_GAP

      module_function

      def calculate(hand)
        catch(:halt) do
          private_instance_methods
            .select { |method| method.match?(SCORE) }
            .sum { |method| send(method, hand) }
        end
      end

      def straight_flush_score(hand)
        if (straight = straight(hand)) && flush?(hand)
          throw(:halt, POINTS[:straight_flush] + straight.last.value)
        else
          0
        end
      end

      def square_score(hand)
        ranks = hand.ranks
        square_cards, _other_cards =
          hand.cards.partition { |card| multiple?(card, ranks, 3) }

        if (square = square_cards.first)
          POINTS[:square] + square.value
        else
          0
        end
      end
      private_class_method :square_score

      def full_house_score(hand)
        ranks = hand.ranks
        three_of_a_kind, other_cards =
          hand.cards.sort.partition { |card| multiple?(card, ranks, 2) }
        return 0 if
          three_of_a_kind.empty? ||
          other_cards.none? { |card| multiple?(card, ranks, 1) }

        throw(:halt, POINTS[:full_house] + three_of_a_kind.first.value)
      end
      private_class_method :full_house_score

      def flush_score(hand)
        flush?(hand) ? POINTS[:flush] + hand.cards.max.value : 0
      end
      private_class_method :flush_score

      def straight_score(hand)
        straight = straight(hand)

        if straight
          POINTS[:straight] + straight.last.value
        else
          0
        end
      end
      private_class_method :straight_score

      def three_of_a_kind_score(hand)
        three_of_a_kind_card = find_multicard(hand, 2).max

        if three_of_a_kind_card
          POINTS[:three_of_a_kind] + three_of_a_kind_card.value
        else
          0
        end
      end
      private_class_method :three_of_a_kind_score

      def two_pair_score(hand)
        pair_cards = find_multicard(hand, 1)
        if pair_cards.length > 1
          POINTS[:two_pair] + pair_cards.max.value
        else
          0
        end
      end
      private_class_method :two_pair_score

      def one_pair_score(hand)
        max_pair_card = find_multicard(hand, 1).max
        max_pair_card ? POINTS[:one_pair] + max_pair_card.value : 0
      end
      private_class_method :one_pair_score

      def high_low_card_score(hand)
        hand.cards.max.value
        # min, max = hand.cards.minmax
        # min.value + max.value
      end
      private_class_method :high_low_card_score

      def find_multicard(hand, floor)
        hand
          .cards
          .each
          .with_object(hand.ranks)
          .with_object(floor)
          .each_with_object([], &method(:add_multicard))
          .uniq
      end
      private_class_method :find_multicard

      def add_multicard(((card, ranks), floor), acc)
        acc << card if multiple?(card, ranks, floor)
      end
      private_class_method :add_multicard

      def multiple?(card, ranks, floor)
        ranks.count(card.rank) > floor
      end
      private_class_method :multiple?

      def straight(hand)
        cards = hand.cards.sort
        numbered_straight(cards) || ace_low_straight(cards)
      end
      private_class_method :straight

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
        numbered_straight(other_cards.prepend(ace_low)) || nil
      end
      private_class_method :ace_low_straight

      def flush?(hand)
        hand.suits.uniq.one?
      end
      private_class_method :flush?
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
    # hands.each { |hand| p hand.send(:score) }
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
      acc[:best_hands] << hand.to_a
    elsif hand > best_hand
      acc[:best_hands] = [hand.to_a]
      acc[:best_hand] = hand
    else
      acc
    end
  end
end
