defmodule Anagram do
  @doc """
  Returns all candidates that are anagrams of, but not equal to, 'base'.
  """
  @spec match(String.t(), [String.t()]) :: [String.t()]
  def match(base, candidates) do
    word = String.downcase(base)
    letters = to_sorted_letters(word)

    candidates
    |> Enum.filter(&anagram?(&1, word, letters))
  end

  defp to_sorted_letters(word) do
    word
    |> String.split("", trim: true)
    |> Enum.sort()
  end

  defp anagram?(candidate, word, letters) do
    candidate_word = String.downcase(candidate)
    word != candidate_word && letters == to_sorted_letters(candidate_word)
  end
end
