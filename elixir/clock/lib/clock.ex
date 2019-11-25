defmodule Clock do
  @minutes_in_hour 60
  @hours_in_day 24

  defstruct hour: 0, minute: 0

  defguardp negative_rollover?(num) when num < 0
  defguardp hour_rollover?(minute) when minute >= @minutes_in_hour
  defguardp day_rollover?(hour) when hour >= @hours_in_day

  @doc """
  Returns a clock that can be represented as a string:

      iex> Clock.new(8, 9) |> to_string
      "08:09"
  """
  @spec new(integer, integer) :: Clock
  def new(hour, minute) when hour_rollover?(minute) do
    new(hour + 1, minute - @minutes_in_hour)
  end

  def new(hour, minute) when negative_rollover?(minute) do
    new(hour - 1, minute + @minutes_in_hour)
  end

  def new(hour, minute) when day_rollover?(hour) do
    new(hour - @hours_in_day, minute)
  end

  def new(hour, minute) when negative_rollover?(hour) do
    new(hour + @hours_in_day, minute)
  end

  def new(hour, minute) do
    %Clock{hour: hour, minute: minute}
  end

  @doc """
  Adds two clock times:

      iex> Clock.new(10, 0) |> Clock.add(3) |> to_string
      "10:03"
  """
  @spec add(Clock, integer) :: Clock
  def add(%Clock{hour: hour, minute: minute}, add_minute) do
    new(hour, minute + add_minute)
  end

  defimpl String.Chars, for: Clock do
    @time_divider ":"
    @zero "0"

    def to_string(%Clock{hour: hour, minute: minute}) do
      [hour, minute]
      |> Enum.map(&to_time_digit/1)
      |> Enum.join(@time_divider)
    end

    defp to_time_digit(number) do
      number
      |> Integer.to_string()
      |> String.pad_leading(2, @zero)
    end
  end
end
