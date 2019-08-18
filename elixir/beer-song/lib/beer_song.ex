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
  @spec lyrics() :: String.t()
  def lyrics, do: lyrics(99..0)

  @spec lyrics(Range.t()) :: String.t()
  def lyrics(range) do
    range
    |> Enum.map(&verse/1)
    |> Enum.join("\n")
  end

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
    default = [container: "bottles", subject: "one"]
    options = Keyword.merge(default, options)

    """
    Take #{options[:subject]} down and pass it around, \
    #{beer_on_wall(amount, options[:container])}.\
    """
  end

  defp beer_on_wall(amount, container \\ "bottles") do
    "#{amount_of_beer(amount, container)} on the wall"
  end

  defp amount_of_beer(amount, container) do
    "#{amount} #{container} of beer"
  end
end
