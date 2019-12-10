defmodule Sieve do
  @doc """
  Generates a list of primes up to a given limit.
  """
  @spec primes_to(non_neg_integer) :: [non_neg_integer]
  def primes_to(limit) do
    limit
    |> mark_composites()
    |> Enum.filter(&(elem(&1, 1) == :prime))
    |> Enum.map(&elem(&1, 0))
    |> Enum.sort()
  end

  defp mark_composites(limit) do
    primes_range = primes_range(limit)
    primes_limit = primes_limit(limit)
    Enum.reduce(primes_limit..2, primes_range, &check_composite(limit, &1, &2))
  end

  defp primes_range(limit) do
    2..limit
    |> Enum.map(&{&1, :prime})
    |> Enum.into(%{})
  end

  defp primes_limit(limit) do
    limit
    |> :math.sqrt()
    |> floor()
  end

  defp check_composite(limit, number, acc) do
    case acc[number] do
      :composite ->
        acc

      :prime ->
        mark_multiples_as_composites(limit, number, acc)
    end
  end

  defp mark_multiples_as_composites(limit, number, acc) do
    limit
    |> multiples(number)
    |> Enum.reduce(acc, &Map.put(&2, &1, :composite))
  end

  defp multiples(limit, number) do
    number
    |> :math.pow(2)
    |> trunc()
    |> :lists.seq(limit, number)
  end
end
