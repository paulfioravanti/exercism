defmodule Isogram do
  @word_characters ~r/\w/

  @doc """
  Determines if a word or sentence is an isogram
  """
  @spec isogram?(String.t()) :: boolean
  def isogram?(sentence) do
    letters =
      sentence
      |> String.downcase()
      |> (fn sentence -> Regex.scan(@word_characters, sentence) end).()

    Enum.uniq(letters) == letters
  end
end
