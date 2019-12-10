defmodule WordCount do
  @non_alphanumeric_or_dashes ~r([^[:alnum:]\-])u

  @doc """
  Count the number of words in the sentence.

  Words are compared case-insensitively.
  """
  @spec count(String.t()) :: map
  def count(sentence) do
    sentence
    |> convert_to_words()
    |> Enum.reduce(%{}, &increment_tally_for_word/2)
  end

  defp convert_to_words(sentence) do
    sentence
    |> String.downcase()
    |> String.split(@non_alphanumeric_or_dashes, trim: true)
  end

  defp increment_tally_for_word(word, acc) do
    Map.update(acc, word, 1, &(&1 + 1))
  end
end
