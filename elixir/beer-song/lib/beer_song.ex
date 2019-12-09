defmodule BeerSong do
  @doc """
  Get a single verse of the beer song
  """
  @spec verse(integer) :: String.t()
  def verse(0) do
    """
    #{line_1("no more")}
    Go to the store and buy some more, #{beer_on_wall(99)}.
    """
  end

  def verse(1) do
    """
    #{line_1(1, "bottle")}
    #{line_2("no more", subject: "it")}
    """
  end

  def verse(2) do
    """
    #{line_1(2)}
    #{line_2(1, container: "bottle")}
    """
  end

  def verse(number) do
    """
    #{line_1(number)}
    #{line_2(number - 1)}
    """
  end

  @doc """
  Get the entire beer song for a given range of numbers of bottles.
  """
  @spec lyrics(Range.t()) :: String.t()
  def lyrics(range \\ 99..0), do: Enum.map_join(range, "\n", &verse/1)

  defp line_1(amount, container \\ "bottles") do
    formatted_amount =
      amount
      |> to_string()
      |> String.capitalize()

    """
    #{beer_on_wall(formatted_amount, container)}, \
    #{amount_of_beer(amount, container)}.\
    """
  end

  defp line_2(amount, options \\ []) do
    subject = Keyword.get(options, :subject, "one")
    container = Keyword.get(options, :container, "bottles")

    """
    Take #{subject} down and pass it around, \
    #{beer_on_wall(amount, container)}.\
    """
  end

  defp beer_on_wall(amount, container \\ "bottles") do
    "#{amount_of_beer(amount, container)} on the wall"
  end

  defp amount_of_beer(amount, container), do: "#{amount} #{container} of beer"
end
