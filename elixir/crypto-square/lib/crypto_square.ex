defmodule CryptoSquare do
  @spaces_and_punctuation ~r/[[:space:][:punct:]]/

  @doc """
  Encode string square methods
  ## Examples

    iex> CryptoSquare.encode("abcd")
    "ac bd"
  """
  @spec encode(String.t()) :: String.t()
  def encode(""), do: ""

  def encode(str) do
    str
    |> normalise()
    |> generate_rectangle()
    |> encode_rectangle()
  end

  defp normalise(str) do
    str
    |> String.downcase()
    |> String.replace(@spaces_and_punctuation, "")
  end

  defp generate_rectangle(normalised_str) do
    num_columns = calculate_num_columns(normalised_str)
    leftover = List.duplicate("", num_columns)

    normalised_str
    |> String.codepoints()
    |> Enum.chunk_every(num_columns, num_columns, leftover)
  end

  defp calculate_num_columns(normalised_str) do
    normalised_str
    |> String.length()
    |> :math.sqrt()
    |> ceil()
  end

  defp encode_rectangle(rectangle) do
    rectangle
    |> transpose()
    |> Enum.join(" ")
  end

  defp transpose(rows) do
    rows
    |> Enum.zip()
    |> Enum.map(&Tuple.to_list/1)
  end
end
