defmodule Prime do
  @minimum_count 1

  @doc """
  Generates the nth prime.
  """
  @spec nth(non_neg_integer) :: non_neg_integer
  def nth(count) when count < @minimum_count, do: raise(ArgumentError)

  def nth(count) do
    primes()
    |> Enum.at(count - 1)
  end

  defp primes do
    Stream.iterate(2, &(&1 + 1))
    |> Stream.filter(&prime?/1)
  end

  defp prime?(2), do: true

  defp prime?(number) do
    2..ceil(:math.sqrt(number))
    |> Enum.all?(&has_remainder?(number, &1))
  end

  defp has_remainder?(number1, number2), do: rem(number1, number2) > 0
end
