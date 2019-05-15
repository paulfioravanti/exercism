# frozen_string_literal: true

require "pry"
class Poker
  class Hand
    class Card
      include Comparable

      RANKS = %w[2 3 4 5 6 7 8 9 10 J Q K A].freeze
      private_constant :RANKS
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
        RANKS.index(rank).fdiv(RANKS.length)
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
      ONE_PAIR_POINTS = 1
      private_constant :ONE_PAIR_POINTS
      SCORE = /score\z/.freeze
      private_constant :SCORE
      TWO_PAIR_POINTS = 2
      private_constant :TWO_PAIR_POINTS

      module_function

      def calculate(hand)
        private_instance_methods
          .select { |method| method[SCORE] }
          .reduce(0) { |acc, attribute| acc + send(attribute, hand) }
      end

      def two_pair_score(hand)
        pair_cards = find_pair_cards(hand)
        if pair_cards.length > 1
          TWO_PAIR_POINTS + pair_cards.max.value
        else
          0
        end
      end
      private_class_method :two_pair_score

      def one_pair_score(hand)
        max_pair_card = find_pair_cards(hand).max
        max_pair_card ? ONE_PAIR_POINTS + max_pair_card.value : 0
      end
      private_class_method :one_pair_score

      def high_card_score(hand)
        hand.cards.max.value
      end
      private_class_method :high_card_score

      def find_pair_cards(hand)
        hand
          .cards
          .find_all { |card| pair?(card, hand) }
          .uniq
      end
      private_class_method :find_pair_cards

      def pair?(card, hand)
        hand.ranks.count(card.rank) > 1
      end
      private_class_method :pair?
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
