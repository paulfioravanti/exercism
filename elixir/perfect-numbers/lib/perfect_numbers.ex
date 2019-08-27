defmodule PerfectNumbers do
  defguardp non_natural_number?(number) when number < 1

  @doc """
  Determine the aliquot sum of the given `number`, by summing all the factors
  of `number`, aside from `number` itself.

  Based on this sum, classify the number as:

  :perfect if the aliquot sum is equal to `number`
  :abundant if the aliquot sum is greater than `number`
  :deficient if the aliquot sum is less than `number`
  """
  @spec classify(number :: integer) :: {:ok, atom} | {:error, String.t()}

  def classify(number) when non_natural_number?(number) do
    {:error, "Classification is only possible for natural numbers."}
  end

  def classify(1), do: {:ok, :deficient}

  def classify(number) do
    aliquot_sum = aliquot_sum(number)

    classification =
      cond do
        aliquot_sum > number ->
          :abundant

        aliquot_sum == number ->
          :perfect

        true ->
          :deficient
      end

    {:ok, classification}
  end

  defp aliquot_sum(number) do
    1..(number - 1)
    |> Enum.reduce(0, &add_factor(number, &1, &2))
  end

  defp add_factor(number, candidate_factor, acc) do
    if factor?(number, candidate_factor) do
      acc + candidate_factor
    else
      acc
    end
  end

  defp factor?(number, candidate_factor) do
    rem(number, candidate_factor) == 0
  end
end
