defmodule Bowling do
  defmodule Game do
    defmodule Frame do
      @strike 10
      @max_pins 10
      @max_rolls 2
      @final_frame 10
      @final_frame_max_rolls 3
      @exceeds_pin_count_message "Pin count exceeds pins on the lane"

      defstruct rolls: [], score: 0, result: :open

      defguardp roll_pins_exceed_max_pins?(
                  first_roll,
                  second_roll,
                  frame_number
                )
                when frame_number < @final_frame and
                       first_roll + second_roll > @max_pins

      defguardp strike?(roll) when roll == @strike

      defguardp spare?(first_roll, second_roll)
                when first_roll + second_roll == @max_pins

      defguardp exceeds_max_pins?(pins) when pins > @max_pins

      def exceeds_pin_count_message, do: @exceeds_pin_count_message

      def new, do: %Frame{}

      def roll(%Frame{rolls: [second_roll, @strike]}, third_roll, @final_frame)
          when not strike?(second_roll) and
                 exceeds_max_pins?(second_roll + third_roll) do
        {:error, @exceeds_pin_count_message}
      end

      def roll(%Frame{rolls: [second_roll, first_roll]}, _third_roll, @final_frame)
          when not strike?(second_roll) and
                 not spare?(first_roll, second_roll) do
        {:error, "Cannot roll after game is over"}
      end

      def roll(%Frame{rolls: [@strike]}, roll, @final_frame)
          when exceeds_max_pins?(roll) do
        {:error, @exceeds_pin_count_message}
      end

      def roll(%Frame{rolls: [first_roll]}, roll, frame_number)
          when roll_pins_exceed_max_pins?(first_roll, roll, frame_number) do
        {:error, @exceeds_pin_count_message}
      end

      def roll(%Frame{rolls: rolls, score: score} = frame, roll, _frame_number) do
        {:ok, %Frame{frame | rolls: [roll | rolls], score: score + roll}}
      end

      def over?(%Frame{rolls: rolls}, frame_number) do
        length(rolls) == max_rolls(frame_number)
      end

      defp max_rolls(@final_frame), do: @final_frame_max_rolls
      defp max_rolls(_frame_number), do: @max_rolls
    end

    @max_frames 10
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

    def roll(%Game{frames: [current_frame | rest] = frames} = game, roll) do
      frame_number = length(frames)

      case Frame.roll(current_frame, roll, frame_number) do
        {:ok, frame} ->
          frames = [frame | rest]

          cond do
            Frame.over?(frame, frame_number) and game_over?(frames) ->
              %Game{game | frames: frames}

            Frame.over?(frame, frame_number) ->
              %Game{game | frames: [Frame.new() | frames]}

            true ->
              %Game{game | frames: frames}
          end

        {:error, message} ->
          {:error, message}
      end
    end

    def score(%Game{frames: frames}) do
      if game_over?(frames) do
        Enum.reduce(frames, 0, &(&2 + &1.score))
      else
        {:error, "Score cannot be taken until the end of the game"}
      end
    end

    defp game_over?(frames), do: length(frames) == @max_frames
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
