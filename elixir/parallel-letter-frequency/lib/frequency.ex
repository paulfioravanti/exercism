defmodule Frequency do
  @initial_letter_count 1
  # REF: https://www.regular-expressions.info/unicode.html
  @unicode_letter ~r/\p{L}/

  @doc """
  Count letter frequency in parallel.

  Returns a map of characters to frequencies.

  The number of worker processes to use can be set with 'workers'.
  """
  @spec frequency([String.t()], pos_integer) :: map
  def frequency([], _workers), do: %{}

  def frequency(texts, workers) do
    texts
    |> Task.async_stream(&tally_letters/1, max_concurrency: workers)
    |> Enum.reduce(%{}, &tally_letter_tally/2)
  end

  defp tally_letters(string) do
    string
    |> String.downcase()
    |> String.graphemes()
    |> Enum.filter(&unicode_letter?/1)
    |> Enum.reduce(%{}, &increment_letter_count/2)
  end

  defp unicode_letter?(letter), do: String.match?(letter, @unicode_letter)

  defp increment_letter_count(letter, acc) do
    Map.update(acc, letter, @initial_letter_count, &(&1 + 1))
  end

  defp tally_letter_tally({:ok, letter_tally}, acc) do
    Map.merge(acc, letter_tally, &tally_letter_count/3)
  end

  defp tally_letter_count(_letter, count1, count2), do: count1 + count2
end
