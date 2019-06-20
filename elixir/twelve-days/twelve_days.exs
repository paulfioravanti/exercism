defmodule TwelveDays do
  @cardinals %{
    "first" => "a",
    "second" => "two",
    "third" => "three",
    "fifth" => "five",
    "eighth" => "eight",
    "ninth" => "nine",
    "twelfth" => "twelve"
  }
  @comma ", "
  @comma_and ", and "
  @gifts [
    " Partridge in a Pear Tree",
    " Turtle Doves",
    " French Hens",
    " Calling Birds",
    " Gold Rings",
    " Geese-a-Laying",
    " Swans-a-Swimming",
    " Maids-a-Milking",
    " Ladies Dancing",
    " Lords-a-Leaping",
    " Pipers Piping",
    " Drummers Drumming"
  ]
  @i_got " day of Christmas my true love gave to me: "
  @on_the "On the "
  @ordinals ~w(
    first
    second
    third
    fourth
    fifth
    sixth
    seventh
    eighth
    ninth
    tenth
    eleventh
    twelfth
  )
  @gifts_for_each_day_of_christmas Enum.zip(@ordinals, @gifts)
  @ordinal_ending ~r/th\z/
  @newline "\n"
  @period "."

  @doc """
  Given a `number`, return the song's verse for that specific day, including
  all gifts for previous days in the same line.
  """
  @spec verse(number :: integer) :: String.t()
  def verse(number) do
    index = number - 1
    ordinal = Enum.at(@ordinals, index)
    amount = cardinal_from_ordinal(ordinal)
    current_gift = Enum.at(@gifts, index)
    extra_gifts = calculate_extra_gifts(index)

    ordinal
    |> declaration_of_receipt()
    |> Kernel.<>(amount)
    |> Kernel.<>(current_gift)
    |> Kernel.<>(extra_gifts)
    |> Kernel.<>(@period)
  end

  @doc """
  Given a `starting_verse` and an `ending_verse`, return the verses for each
  included day, one per line.
  """
  @spec verses(starting_verse :: integer, ending_verse :: integer) :: String.t()
  def verses(starting_verse, ending_verse) do
    starting_verse..ending_verse
    |> Enum.map(&verse/1)
    |> Enum.join(@newline)
  end

  @doc """
  Sing all 12 verses, in order, one verse per line.
  """
  @spec sing() :: String.t()
  def sing do
    verses(1, 12)
  end

  defp declaration_of_receipt(day) do
    @on_the <> day <> @i_got
  end

  defp cardinal_from_ordinal(ordinal) do
    @cardinals
    |> Map.get(ordinal, remove_ordinal_ending(ordinal))
  end

  defp remove_ordinal_ending(ordinal) do
    ordinal
    |> String.replace(@ordinal_ending, "")
  end

  defp calculate_extra_gifts(index) do
    @gifts_for_each_day_of_christmas
    |> Enum.take(index)
    |> Enum.reduce([], &add_gift/2)
    |> Enum.join()
  end

  defp add_gift({ordinal, gift}, acc) do
    amount = cardinal_from_ordinal(ordinal)

    case acc do
      [] ->
        [@comma_and <> amount <> gift]

      _ ->
        [@comma <> amount <> gift | acc]
    end
  end
end
