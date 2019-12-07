defmodule Bowling do
  defmodule Game do
    defmodule Frame do
      @strike 10
      @max_pins 10
      @max_rolls 2
      @final_frame 10
      @final_frame_max_rolls 3
      @exceeds_pin_count_message "Pin count exceeds pins on the lane"

      defstruct rolls: [], score: 0

      defguardp strike?(roll) when roll == @strike

      defguardp spare?(first_roll, second_roll)
                when first_roll + second_roll == @max_pins

      defguardp open?(first_roll, second_roll)
                when first_roll + second_roll < @max_pins

      defguardp final_frame?(frames) when length(frames) == @final_frame
      defguardp standard_frame?(frames) when length(frames) < @final_frame
      defguardp exceeds_max_pins?(pins) when pins > @max_pins

      defguardp exceeds_max_pins?(roll1, roll2)
                when exceeds_max_pins?(roll1 + roll2)

      defguardp invalid_post_strike_bonus_balls?(second_roll, third_roll)
                when not strike?(second_roll) and
                       exceeds_max_pins?(second_roll, third_roll)

      defguardp final_frame_over?(first_roll, second_roll)
                when not (strike?(first_roll) or
                            strike?(second_roll) or
                            spare?(first_roll, second_roll))

      def exceeds_pin_count_message, do: @exceeds_pin_count_message

      def new, do: %Frame{}

      def roll(
            [%Frame{rolls: [second_roll, first_roll]} | _rest] = frames,
            _third_roll
          )
          when final_frame?(frames) and
                 final_frame_over?(first_roll, second_roll) do
        {:error, "Cannot roll after game is over"}
      end

      def roll(
            [%Frame{rolls: [second_roll, @strike]} | _rest] = frames,
            third_roll
          )
          when final_frame?(frames) and
                 invalid_post_strike_bonus_balls?(second_roll, third_roll) do
        {:error, @exceeds_pin_count_message}
      end

      def roll([%Frame{rolls: [@strike]} | _rest] = frames, second_roll)
          when final_frame?(frames) and exceeds_max_pins?(second_roll) do
        {:error, @exceeds_pin_count_message}
      end

      def roll([%Frame{rolls: [first_roll]} | _rest] = frames, second_roll)
          when standard_frame?(frames) and
                 exceeds_max_pins?(first_roll, second_roll) do
        {:error, @exceeds_pin_count_message}
      end

      def roll(
            [%Frame{rolls: [@strike, @strike], score: score} | rest] = frames,
            @strike
          )
          when final_frame?(frames) do
        current_frame = %Frame{
          rolls: [@strike, @strike, @strike],
          score: score + @strike
        }

        frames = [current_frame | rest]
        {:ok, frames}
      end

      def roll(
            [
              %Frame{rolls: [@strike], score: score},
              %Frame{rolls: [@strike], score: previous_score} = previous_frame
              | rest
            ] = frames,
            @strike
          )
          when final_frame?(frames) do
        previous_frame = %Frame{
          previous_frame
          | score: previous_score + @strike
        }

        current_frame = %Frame{
          rolls: [@strike, @strike],
          score: score + @strike
        }

        frames = [current_frame, previous_frame | rest]
        {:ok, frames}
      end

      def roll(
            [
              %Frame{rolls: []},
              %Frame{rolls: [@strike], score: previous_score} = previous_frame,
              %Frame{rolls: [@strike], score: second_previous_score} =
                second_previous_frame
              | rest
            ],
            @strike
          ) do
        second_previous_frame = %Frame{
          second_previous_frame
          | score: second_previous_score + @strike
        }

        previous_frame = %Frame{
          previous_frame
          | score: previous_score + @strike
        }

        current_frame = %Frame{rolls: [@strike], score: @strike}
        frames = [current_frame, previous_frame, second_previous_frame | rest]
        {:ok, frames}
      end

      def roll(
            [
              %Frame{rolls: [first_roll], score: score},
              %Frame{rolls: [@strike], score: previous_score} = previous_frame,
              %Frame{rolls: [@strike], score: second_previous_score} =
                second_previous_frame
              | rest
            ],
            second_roll
          ) do
        second_previous_frame = %Frame{
          second_previous_frame
          | score: second_previous_score + first_roll
        }

        previous_frame = %Frame{
          previous_frame
          | score: previous_score + first_roll + second_roll
        }

        current_frame = %Frame{
          rolls: [second_roll, first_roll],
          score: score + second_roll
        }

        frames = [current_frame, previous_frame, second_previous_frame | rest]
        {:ok, frames}
      end

      def roll(
            [
              %Frame{rolls: []},
              %Frame{rolls: [@strike], score: previous_score} = previous_frame
              | rest
            ],
            @strike
          ) do
        previous_frame = %Frame{
          previous_frame
          | score: previous_score + @strike
        }

        current_frame = %Frame{rolls: [@strike], score: @strike}
        frames = [current_frame, previous_frame | rest]
        {:ok, frames}
      end

      def roll(
            [
              %Frame{rolls: [first_roll], score: score},
              %Frame{rolls: [@strike], score: previous_score} = previous_frame
              | rest
            ],
            second_roll
          ) do
        previous_frame = %Frame{
          previous_frame
          | score: previous_score + first_roll + second_roll
        }

        current_frame = %Frame{
          rolls: [second_roll, first_roll],
          score: score + second_roll
        }

        frames = [current_frame, previous_frame | rest]
        {:ok, frames}
      end

      def roll(
            [
              %Frame{rolls: []},
              %Frame{
                rolls: [previous_second_roll, previous_first_roll],
                score: previous_score
              } = previous_frame
              | rest
            ],
            first_roll
          )
          when spare?(previous_first_roll, previous_second_roll) do
        previous_frame = %Frame{
          previous_frame
          | score: previous_score + first_roll
        }

        current_frame = %Frame{rolls: [first_roll], score: first_roll}
        {:ok, [current_frame, previous_frame | rest]}
      end

      def roll([%Frame{rolls: rolls, score: score} | rest], roll) do
        frame = %Frame{rolls: [roll | rolls], score: score + roll}
        {:ok, [frame | rest]}
      end

      def over?([%Frame{rolls: [@strike]} | _rest] = frames)
          when final_frame?(frames) do
        false
      end

      def over?([%Frame{rolls: [second_roll, first_roll]} | _rest] = frames)
          when final_frame?(frames) and open?(first_roll, second_roll) do
        true
      end

      def over?([%Frame{rolls: [@strike]} | _rest]), do: true

      def over?([%Frame{rolls: rolls} | _rest] = frames)
          when final_frame?(frames) do
        length(rolls) == @final_frame_max_rolls
      end

      def over?([%Frame{rolls: rolls} | _rest]), do: length(rolls) == @max_rolls

      def accumulate_score(%Frame{score: score}, acc), do: acc + score

      def final?(frames), do: final_frame?(frames)
    end

    @min_roll_points 0
    @max_roll_points 10

    defstruct frames: []

    defguardp negative_roll?(roll) when roll < @min_roll_points
    defguardp exceeds_pins?(roll) when roll > @max_roll_points

    def new, do: %Game{frames: [Frame.new()]}

    def roll(_game, roll) when negative_roll?(roll) do
      {:error, "Negative roll is invalid"}
    end

    def roll(_game, roll) when exceeds_pins?(roll) do
      {:error, Frame.exceeds_pin_count_message()}
    end

    def roll(%Game{frames: frames} = game, roll) do
      case Frame.roll(frames, roll) do
        {:ok, frames} ->
          update_game(game, frames)

        {:error, message} ->
          {:error, message}
      end
    end

    def score(%Game{frames: frames}) do
      if game_over?(frames) do
        Enum.reduce(frames, 0, &Frame.accumulate_score/2)
      else
        {:error, "Score cannot be taken until the end of the game"}
      end
    end

    defp update_game(game, frames) do
      cond do
        game_over?(frames) ->
          %Game{game | frames: frames}

        Frame.over?(frames) ->
          %Game{game | frames: [Frame.new() | frames]}

        true ->
          %Game{game | frames: frames}
      end
    end

    defp game_over?(frames), do: Frame.final?(frames) and Frame.over?(frames)
  end

  @doc """
    Creates a new game of bowling that can be used to store the results of
    the game
  """
  @spec start() :: any
  def start, do: Game.new()

  @doc """
    Records the number of pins knocked down on a single roll. Returns `any`
    unless there is something wrong with the given number of pins, in which
    case it returns a helpful message.
  """
  @spec roll(any, integer) :: any | String.t()
  def roll(game, roll), do: Game.roll(game, roll)

  @doc """
    Returns the score of a given game of bowling if the game is complete.
    If the game isn't complete, it returns a helpful message.
  """
  @spec score(any) :: integer | String.t()
  def score(game), do: Game.score(game)
end
