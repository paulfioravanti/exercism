defmodule TwelveDays do
  @cardinals %{
    "first" => "a"
  }
  @gifts %{
    "first" => " Partridge in a Pear Tree"
  }
  @ordinals %{
    1 => "first"
  }
  @period "."

  @doc """
  Given a `number`, return the song's verse for that specific day, including
  all gifts for previous days in the same line.
  """
  @spec verse(number :: integer) :: String.t()
  def verse(number) do
    ordinal =
      @ordinals
      |> Map.get(number)

    ordinal
    |> declaration_of_receipt()
    |> Kernel.<>(Map.get(@cardinals, ordinal))
    |> Kernel.<>(Map.get(@gifts, ordinal))
    |> Kernel.<>(@period)
  end

  @doc """
  Given a `starting_verse` and an `ending_verse`, return the verses for each
  included day, one per line.
  """
  @spec verses(starting_verse :: integer, ending_verse :: integer) :: String.t()
  def verses(starting_verse, ending_verse) do
  end

  @doc """
  Sing all 12 verses, in order, one verse per line.
  """
  @spec sing() :: String.t()
  def sing do
  end

  defp declaration_of_receipt(day) do
    "On the " <> day <> " day of Christmas my true love gave to me: "
  end
end
