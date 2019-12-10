defmodule Pangram do
  @non_ascii_letters ~r([^a-z])
  @number_of_letters_in_alphabet 26

  @doc """
  Determines if a word or sentence is a pangram.
  A pangram is a sentence using every letter of the alphabet at least once.

  Returns a boolean.

    ## Examples

      iex> Pangram.pangram?("the quick brown fox jumps over the lazy dog")
      true

  """

  @spec pangram?(String.t()) :: boolean
  def pangram?(sentence) do
    sentence
    |> String.downcase()
    |> String.replace(@non_ascii_letters, "")
    |> String.to_charlist()
    |> Enum.uniq()
    |> Enum.count()
    |> Kernel.==(@number_of_letters_in_alphabet)
  end
end
