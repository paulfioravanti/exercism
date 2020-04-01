defmodule RomanNumerals do
  @moduledoc false

  @roman_numerals [
    {"M", 1000},
    {"CM", 900},
    {"D", 500},
    {"CD", 400},
    {"C", 100},
    {"XC", 90},
    {"L", 50},
    {"XL", 40},
    {"X", 10},
    {"IX", 9},
    {"V", 5},
    {"IV", 4},
    {"I", 1}
  ]

  @doc """
  Convert the number to a roman number.
  """
  @spec numeral(pos_integer) :: String.t()
  def numeral(number) do
    @roman_numerals
    |> Enum.reduce({"", number}, &append_roman_numeral/2)
    |> elem(0)
  end

  defp append_roman_numeral({roman, decimal}, {numeral, number}) do
    quotient = div(number, decimal)
    remainder = rem(number, decimal)
    string = numeral <> String.duplicate(roman, quotient)
    {string, remainder}
  end
end
