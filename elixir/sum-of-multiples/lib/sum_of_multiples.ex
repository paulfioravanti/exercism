defmodule SumOfMultiples do
  @doc """
  Adds up all numbers from 1 to a given end number that are multiples of the factors provided.
  """
  @spec to(non_neg_integer, [non_neg_integer]) :: non_neg_integer
  def to(limit, factors) do
    1..(limit - 1)
    |> Stream.filter(&multiples?(factors, &1))
    |> Enum.sum()
  end

  defp multiples?(factors, range_number) do
    Enum.any?(factors, &multiple?(range_number, &1))
  end

  defp multiple?(range_number, factor), do: rem(range_number, factor) == 0
end
