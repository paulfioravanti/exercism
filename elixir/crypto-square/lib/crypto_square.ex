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
    |> normalise_input()
    |> generate_rectangle()
    |> encode_rectangle()
  end

  defp normalise_input(str) do
    str
    |> String.downcase()
    |> String.replace(@spaces_and_punctuation, "")
  end

  defp generate_rectangle(normalised_str) do
    letters = String.codepoints(normalised_str)
    num_columns = calculate_num_columns(normalised_str)
    padding = generate_padding(letters, num_columns)

    letters
    |> Enum.concat(padding)
    |> Enum.chunk_every(num_columns)
  end

  defp calculate_num_columns(normalised_str) do
    normalised_str
    |> String.length()
    |> :math.sqrt()
    |> ceil()
  end

  defp generate_padding(letters, num_columns) do
    padding_length = calculate_padding_length(letters, num_columns)
    List.duplicate(" ", padding_length)
  end

  defp calculate_padding_length(letters, num_columns) do
    letters
    |> length()
    |> Integer.mod(num_columns)
    |> (fn modulo -> num_columns - modulo end).()
    |> Integer.mod(num_columns)
  end

  defp encode_rectangle(rectangle) do
    rectangle
    |> transpose()
    |> Enum.map(&trim_join/1)
    |> Enum.join(" ")
  end

  defp transpose(rows) do
    rows
    |> Enum.zip()
    |> Enum.map(&Tuple.to_list/1)
  end

  defp trim_join(row) do
    row
    |> Enum.join()
    |> String.trim_trailing()
  end
end
