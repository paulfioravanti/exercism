defmodule Binary do
  @binary_string ~r/\A[01]+\z/
  @base 2

  @doc """
  Convert a string containing a binary number to an integer.

  On errors returns 0.
  """
  @spec to_decimal(String.t()) :: non_neg_integer
  def to_decimal(string) do
    if String.match?(string, @binary_string) do
      string
      |> String.codepoints()
      |> Enum.reverse()
      |> Enum.with_index()
      |> Enum.reduce(0, &add_decimal_value/2)
    else
      0
    end
  end

  defp add_decimal_value({bit, index}, acc) do
    acc + String.to_integer(bit) * :math.pow(@base, index)
  end
end
