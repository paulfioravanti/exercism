defmodule PrimeFactors do
  @minimum_prime 2
  @final_factor 1

  defguardp no_remainder?(number, quotient) when rem(number, quotient) == 0

  defguardp final_factor?(number, quotient)
            when @final_factor in [number, quotient]

  @doc """
  Compute the prime factors for 'number'.

  The prime factors are prime numbers that when multiplied give the desired
  number.

  The prime factors of 'number' will be ordered lowest to highest.
  """
  @spec factors_for(pos_integer) :: [pos_integer]
  def factors_for(1), do: []
  def factors_for(number), do: factors_for(number, @minimum_prime, [])

  defp factors_for(number, quotient, acc)
       when final_factor?(number, quotient) do
    Enum.reverse(acc)
  end

  defp factors_for(number, quotient, acc)
       when no_remainder?(number, quotient) do
    number
    |> div(quotient)
    |> factors_for(quotient, [quotient | acc])
  end

  defp factors_for(number, quotient, acc) do
    factors_for(number, quotient + 1, acc)
  end
end
