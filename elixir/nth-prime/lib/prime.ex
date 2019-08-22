defmodule Prime do
  @doc """
  Generates the nth prime.
  """
  @spec nth(non_neg_integer) :: non_neg_integer
  def nth(count) when count > 0 do
    primes()
    |> Enum.at(count - 1)
  end

  defp primes do
    Stream.iterate(2, &(&1 + 1))
    |> Stream.reject(&composite?/1)
  end

  defp composite?(2), do: false

  defp composite?(number) do
    2..(number - 1)
    |> Enum.any?(&no_remainder?(number, &1))
  end

  defp no_remainder?(number1, number2), do: rem(number1, number2) == 0
end
