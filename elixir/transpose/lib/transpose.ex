defmodule Transpose do
  @doc """
  Given an input text, output it transposed.

  Rows become columns and columns become rows. See https://en.wikipedia.org/wiki/Transpose.

  If the input has rows of different lengths, this is to be solved as follows:
    * Pad to the left with spaces.
    * Don't pad to the right.

  ## Examples
  iex> Transpose.transpose("ABC\nDE")
  "AD\nBE\nC"

  iex> Transpose.transpose("AB\nDEF")
  "AD\nBE\n F"
  """

  @spec transpose(String.t()) :: String.t()
  def transpose(""), do: ""

  def transpose(input) do
    rows = String.split(input, "\n")
    max_row_width = max_row_width(rows)

    rows
    |> Enum.map(&generate_right_padded_row(&1, max_row_width))
    |> transpose_rows()
    |> Enum.join("\n")
    |> String.trim_trailing()
  end

  defp max_row_width(rows) do
    rows
    |> Enum.map(&String.length/1)
    |> Enum.max()
  end

  defp generate_right_padded_row(row, max_row_width) do
    row
    |> String.pad_trailing(max_row_width)
    |> String.codepoints()
  end

  defp transpose_rows(rows) do
    rows
    |> Enum.zip()
    |> Enum.map(&Tuple.to_list/1)
  end
end
