defmodule Isogram do
  @word_characters ?A..?z

  @doc """
  Determines if a word or sentence is an isogram
  """
  @spec isogram?(String.t()) :: boolean
  def isogram?(sentence) do
    letters =
      for <<letter <- sentence>>,
          letter in @word_characters,
          do: letter

    Enum.uniq(letters) == letters
  end
end
