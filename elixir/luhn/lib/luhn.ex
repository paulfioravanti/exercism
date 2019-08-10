defmodule Luhn do
  @two_or_more_digits_only ~r/\A\d{2,}\z/

  @doc """
  Checks if the given number is valid via the luhn formula
  """
  @spec valid?(String.t()) :: boolean
  def valid?(number) do
    stripped_string = String.replace(number, " ", "")

    if valid_format?(stripped_string) do
      valid_number?(stripped_string)
    else
      false
    end
  end

  defp valid_format?(stripped_string) do
    String.match?(stripped_string, @two_or_more_digits_only)
  end

  defp valid_number?(stripped_string) do
    stripped_string
    |> convert_string_to_reversed_numbers()
    |> Enum.chunk_every(2)
    |> Enum.reduce(0, &calculate_luhn_value/2)
    |> evenly_divisible_by_10?()
  end

  defp convert_string_to_reversed_numbers(string) do
    string
    |> String.reverse()
    |> String.codepoints()
    |> Enum.map(&String.to_integer/1)
  end

  defp calculate_luhn_value([left_value, right_value], acc) do
    right_luhn_value =
      right_value
      |> double()
      |> subtract_9_if_greater_than_9()

    acc + left_value + right_luhn_value
  end

  defp calculate_luhn_value([left_value], acc), do: acc + left_value

  defp double(number), do: number * 2

  defp subtract_9_if_greater_than_9(number) do
    if number > 9, do: number - 9, else: number
  end

  defp evenly_divisible_by_10?(number), do: rem(number, 10) == 0
end
