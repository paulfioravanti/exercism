defmodule Hexadecimal do
  @hex_digits '0123456789ABCDEF'
  @hex_string ~r/\A[#{@hex_digits}]+\z/
  @base 16
  @invalid_value 0

  @doc """
    Accept a string representing a hexadecimal value and returns the
    corresponding decimal value.
    It returns the integer 0 if the hexadecimal is invalid.
    Otherwise returns an integer representing the decimal value.

    ## Examples

      iex> Hexadecimal.to_decimal("invalid")
      0

      iex> Hexadecimal.to_decimal("af")
      175

  """
  @spec to_decimal(binary) :: integer
  def to_decimal(hex) do
    hex = String.upcase(hex)

    if String.match?(hex, @hex_string) do
      hex
      |> String.to_charlist()
      |> Enum.reverse()
      |> Enum.with_index()
      |> Enum.reduce(0, &add_hex_value/2)
    else
      @invalid_value
    end
  end

  defp add_hex_value({hex_char, index}, acc) do
    acc +
      Enum.find_index(@hex_digits, &(&1 == hex_char)) *
        :math.pow(@base, index)
  end
end
