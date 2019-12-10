defmodule Luhn do
  @two_or_more_digits_only ~r/\A\d{2,}\z/

  @doc """
  Checks if the given number is valid via the luhn formula
  """
  @spec valid?(String.t()) :: boolean
  def valid?(number) do
    number = String.replace(number, " ", "")
    valid_format?(number) and valid_value?(number)
  end

  defp valid_format?(number) do
    String.match?(number, @two_or_more_digits_only)
  end

  defp valid_value?(number) do
    number
    |> to_reversed_numbers()
    |> Enum.chunk_every(2)
    |> Enum.reduce(0, &add_luhn_value/2)
    |> evenly_divisible_by_10?()
  end

  defp to_reversed_numbers(number) do
    number
    |> String.reverse()
    |> String.codepoints()
    |> Enum.map(&String.to_integer/1)
  end

  defp add_luhn_value([left_value, right_value], acc) do
    right_luhn_value =
      right_value
      |> double()
      |> subtract_9_if_greater_than_9()

    acc + left_value + right_luhn_value
  end

  defp add_luhn_value([left_value], acc), do: acc + left_value

  defp double(number), do: number * 2

  defp subtract_9_if_greater_than_9(number) do
    if number > 9, do: number - 9, else: number
  end

  defp evenly_divisible_by_10?(number), do: rem(number, 10) == 0
end
