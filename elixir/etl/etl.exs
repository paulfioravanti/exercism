defmodule ETL do
  @doc """
  Transform an index into an inverted index.

  ## Examples

  iex> ETL.transform(%{"a" => ["ABILITY", "AARDVARK"], "b" => ["BALLAST", "BEAUTY"]})
  %{"ability" => "a", "aardvark" => "a", "ballast" => "b", "beauty" =>"b"}
  """
  @spec transform(map) :: map
  def transform(input) do
    input
    |> Enum.reduce(%{}, &transform_words/2)
  end

  defp transform_words({point_value, words}, new_scores) do
    words
    |> Enum.reduce(new_scores, &score_by_word(point_value, &1, &2))
  end

  defp score_by_word(point_value, word, new_scores) do
    word_key = String.downcase(word)

    new_scores
    |> Map.put(word_key, point_value)
  end
end
