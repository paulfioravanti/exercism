defmodule Poker do
  defmodule Card do
    # look ahead to non-digit character
    @suit ~r/(?=\D)/
    @ranks ~w[a 2 3 4 5 6 7 8 9 10 J Q K A]
    @num_ranks length(@ranks)
    # Assign a value to a card of less than 1 dependent on its rank
    # so that:
    # - cards can be compared to each other
    # - the value can be tallied along with hand scores
    @single_card_value Decimal.new("0.01")

    defstruct [:rank, :suit]

    def new(card_string) do
      [rank, suit] = String.split(card_string, @suit, trim: true)
      %Card{rank: rank, suit: suit}
    end

    def single_card_value, do: @single_card_value
    def num_ranks, do: @num_ranks

    def to_string(%Card{rank: rank, suit: suit}), do: rank <> suit

    def value(%Card{rank: rank}) do
      @ranks
      |> Enum.find_index(&(&1 == rank))
      |> Decimal.cast()
      |> Decimal.mult(@single_card_value)
    end

    def compare_value(card, {min, max} = acc) do
      cond do
        Decimal.lt?(value(card), value(min)) ->
          {card, max}

        Decimal.gt?(value(card), value(max)) ->
          {min, card}

        true ->
          acc
      end
    end

    def equal?(%Card{rank: rank1}, %Card{rank: rank2}), do: rank1 == rank2

    def multiple?(card, ranks, num_cards) do
      Enum.count(ranks, &(&1 == card.rank)) >= num_cards
    end
  end

  defmodule Hand do
    alias Poker.Score

    defstruct cards: [], score: 0

    def new(hand) do
      cards = Enum.map(hand, &Card.new/1)
      Score.calculate(%Hand{cards: cards})
    end

    def to_list(%Hand{cards: cards}), do: Enum.map(cards, &Card.to_string/1)

    def equal?(%Hand{score: score1}, %Hand{score: score2}) do
      Decimal.equal?(score1, score2)
    end

    def better?(%Hand{score: score1}, %Hand{score: score2}) do
      Decimal.gt?(score1, score2)
    end

    def min_max(%Hand{cards: [first | rest]}) do
      Enum.reduce(rest, {first, first}, &Card.compare_value/2)
    end

    def ranks(%Hand{cards: cards}), do: Enum.map(cards, & &1.rank)
    def suits(%Hand{cards: cards}), do: Enum.map(cards, & &1.suit)

    def find_multicard(%Hand{cards: cards} = hand, num_cards) do
      cards
      |> Enum.reduce([], &add_multicard({ranks(hand), num_cards}, &1, &2))
      |> Enum.uniq_by(& &1.rank)
    end

    defp add_multicard({ranks, num_cards}, card, acc) do
      if Card.multiple?(card, ranks, num_cards), do: [card | acc], else: acc
    end
  end

  defmodule Score do
    defmodule HighLowCard do
      def score(hand) do
        {min, max} = Hand.min_max(hand)

        min
        |> Card.value()
        |> Decimal.div(2)
        |> Decimal.add(Card.value(max))
      end
    end

    defmodule OnePair do
      @points 1
      @pair 2

      def score(hand) do
        hand
        |> Hand.find_multicard(@pair)
        |> tally_points()
      end

      defp tally_points([]), do: 0

      defp tally_points(cards) do
        cards
        |> Enum.max_by(& &1.rank)
        |> Card.value()
        |> Decimal.add(@points)
      end
    end

    defmodule TwoPair do
      @points 2
      @pair 2

      def score(hand) do
        hand
        |> Hand.find_multicard(@pair)
        |> tally_points()
      end

      defp tally_points([]), do: 0
      defp tally_points([_card]), do: 0

      defp tally_points(cards) do
        cards
        |> Enum.max_by(& &1.rank)
        |> Card.value()
        |> Decimal.add(@points)
      end
    end

    defmodule ThreeOfAKind do
      @points 3
      @triplet 3

      def score(hand) do
        hand
        |> Hand.find_multicard(@triplet)
        |> tally_points(hand)
      end

      defp tally_points([], _hand), do: 0

      defp tally_points([card], hand) do
        remaining_cards_score = remaining_cards_score(hand, card)

        card
        |> Card.value()
        |> Decimal.add(@points)
        |> Decimal.add(remaining_cards_score)
      end

      defp remaining_cards_score(%Hand{cards: cards}, card) do
        threes = Enum.filter(cards, &Card.equal?(&1, card))

        %Hand{cards: cards -- threes}
        |> HighLowCard.score()
      end
    end

    defmodule Straight do
      @points 5
      @high_ace "A"
      @low_ace "a"

      def score(hand) do
        hand
        |> straight()
        |> tally_points()
      end

      def straight(hand) do
        cards = Enum.sort_by(hand.cards, &Card.value(&1))
        numbered_straight(cards) || ace_low_straight(cards)
      end

      defp tally_points(nil), do: 0

      defp tally_points(cards) do
        cards
        |> Enum.reverse()
        |> hd()
        |> Card.value()
        |> Decimal.mult(Card.num_ranks())
        |> Decimal.add(@points)
      end

      defp numbered_straight(cards) do
        straight =
          cards
          |> Enum.chunk_every(2, 1, :discard)
          |> Enum.all?(&single_card_gap?/1)

        if straight, do: cards, else: nil
      end

      defp single_card_gap?([card1, card2]) do
        card2
        |> Card.value()
        |> Decimal.sub(Card.value(card1))
        |> Decimal.equal?(Card.single_card_value())
      end

      defp ace_low_straight(cards) do
        cards
        |> Enum.split_with(&(&1.rank == @high_ace))
        |> check_ace_low_straight()
      end

      defp check_ace_low_straight({[ace], other_cards}) do
        ace_low = Card.new(@low_ace <> ace.suit)
        numbered_straight([ace_low | other_cards])
      end

      defp check_ace_low_straight(_other), do: nil
    end

    defmodule Flush do
      @points 8

      def score(hand) do
        if flush?(hand) do
          hand.cards
          |> Enum.sort_by(&Card.value(&1), &>=/2)
          |> hd()
          |> Card.value()
          |> Decimal.add(@points)
        else
          0
        end
      end

      def flush?(hand) do
        hand
        |> Hand.suits()
        |> Enum.uniq()
        |> length()
        |> Kernel.==(1)
      end
    end

    defmodule FullHouse do
      @points 13
      @triplet 3
      @pair 2

      def score(hand) do
        ranks = Hand.ranks(hand)
        {three_set, other_cards} = split_hand(hand, ranks)

        if full_house?(three_set, other_cards, ranks) do
          score =
            three_set
            |> hd()
            |> Card.value()
            |> Decimal.mult(Card.num_ranks())
            |> Decimal.add(@points)
            |> Decimal.add(hand.score)

          throw({:halt, %Hand{hand | score: score}})
        else
          0
        end
      end

      defp split_hand(%Hand{cards: cards}, ranks) do
        cards
        |> Enum.sort_by(&Card.value(&1))
        |> Enum.split_with(&Card.multiple?(&1, ranks, @triplet))
      end

      defp full_house?([], _pair, _ranks), do: false

      defp full_house?(_triplet, pair, ranks) do
        Enum.all?(pair, &Card.multiple?(&1, ranks, @pair))
      end
    end

    defmodule FourOfAKind do
      @points 21
      @square 4

      def score(hand) do
        hand
        |> split_hand()
        |> tally_points(hand)
      end

      defp split_hand(%Hand{cards: cards} = hand) do
        ranks = Hand.ranks(hand)
        Enum.split_with(cards, &Card.multiple?(&1, ranks, @square))
      end

      defp tally_points({[], _cards}, _hand), do: 0

      defp tally_points({square, _other_card}, hand) do
        score =
          square
          |> hd()
          |> Card.value()
          |> Decimal.add(@points)
          |> Decimal.add(hand.score)

        throw({:halt, %Hand{hand | score: score}})
      end
    end

    defmodule StraightFlush do
      alias Poker.Score.{Straight, Flush}

      @points 34

      def score(hand) do
        straight = Straight.straight(hand)

        if straight && Flush.flush?(hand) do
          score =
            straight
            |> Enum.reverse()
            |> hd()
            |> Card.value()
            |> Decimal.add(@points)
            |> Decimal.add(hand.score)

          throw({:halt, %Hand{hand | score: score}})
        else
          0
        end
      end
    end

    @hand_types [
      HighLowCard,
      OnePair,
      TwoPair,
      ThreeOfAKind,
      Straight,
      Flush,
      FullHouse,
      FourOfAKind,
      StraightFlush
    ]

    alias Poker.Hand

    def calculate(hand) do
      Enum.reduce(@hand_types, hand, &sum_points/2)
    catch
      {:halt, hand} ->
        hand
    end

    defp sum_points(module, hand) do
      score =
        module
        |> apply(:score, [hand])
        |> Decimal.add(hand.score)

      %Hand{hand | score: score}
    end
  end

  @doc """
  Given a list of poker hands, return a list containing the highest scoring hand.

  If two or more hands tie, return the list of tied hands in the order they were received.

  The basic rules and hand rankings for Poker can be found at:

  https://en.wikipedia.org/wiki/List_of_poker_hands

  For this exercise, we'll consider the game to be using no Jokers,
  so five-of-a-kind hands will not be tested. We will also consider
  the game to be using multiple decks, so it is possible for multiple
  players to have identical cards.

  Aces can be used in low (A 2 3 4 5) or high (10 J Q K A) straights, but do not count as
  a high card in the former case.

  For example, (A 2 3 4 5) will lose to (2 3 4 5 6).

  You can also assume all inputs will be valid, and do not need to perform error checking
  when parsing card values. All hands will be a list of 5 strings, containing a number
  (or letter) for the rank, followed by the suit.

  Ranks (lowest to highest): 2 3 4 5 6 7 8 9 10 J Q K A
  Suits (order doesn't matter): C D H S

  Example hand: ~w(4S 5H 4C 5D 4H) # Full house, 5s over 4s
  """
  @spec best_hand(list(list(String.t()))) :: list(list(String.t()))
  def best_hand([hand]), do: [hand]

  def best_hand(hands) do
    [first_hand | rest] = Enum.map(hands, &Hand.new/1)

    rest
    |> Enum.reduce(initial_best_hands(first_hand), &compare_best_hand/2)
    |> Map.fetch!(:best_hands)
    |> Enum.reverse()
  end

  defp initial_best_hands(hand) do
    %{best_hands: [Hand.to_list(hand)], best_hand: hand}
  end

  defp compare_best_hand(hand, %{best_hand: best_hand} = acc) do
    cond do
      Hand.equal?(hand, best_hand) ->
        Map.update!(acc, :best_hands, &[Hand.to_list(hand) | &1])

      Hand.better?(hand, best_hand) ->
        acc
        |> Map.put(:best_hands, [Hand.to_list(hand)])
        |> Map.put(:best_hand, hand)

      true ->
        acc
    end
  end
end
