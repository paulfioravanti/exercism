defmodule Anagram do
  @doc """
  Returns all candidates that are anagrams of, but not equal to, 'base'.
  """
  @spec match(String.t(), [String.t()]) :: [String.t()]
  def match(base, candidates) do
    word = String.downcase(base)
    letters = to_sorted_letters(word)
    Enum.filter(candidates, &anagram?(String.downcase(&1), word, letters))
  end

  defp to_sorted_letters(word) do
    word
    |> String.graphemes()
    |> Enum.sort()
  end

  defp anagram?(candidate, word, letters) do
    not same_word?(word, candidate) and same_letters?(letters, candidate)
  end

  defp same_word?(word, candidate), do: word == candidate

  defp same_letters?(letters, candidate) do
    letters == to_sorted_letters(candidate)
  end
end
