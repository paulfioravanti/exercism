defmodule SumOfMultiples do
  @doc """
  Adds up all numbers from 1 to a given end number that are multiples of the factors provided.
  """
  @spec to(non_neg_integer, [non_neg_integer]) :: non_neg_integer
  def to(limit, factors) do
    limit
    |> multiple_range()
    |> Stream.filter(&multiples?(factors, &1))
    |> Enum.sum()
  end

  defp multiple_range(limit) do
    1..(limit - 1)
  end

  defp multiples?(factors, range_number) do
    factors
    |> Enum.any?(&multiple?(range_number, &1))
  end

  defp multiple?(range_number, factor) do
    Integer.mod(range_number, factor) == 0
  end
end
