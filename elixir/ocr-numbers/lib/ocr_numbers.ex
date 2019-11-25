defmodule OcrNumbers do
  @conversions %{
    " _ | ||_|   " => "0",
    "     |  |   " => "1",
    " _  _||_    " => "2",
    " _  _| _|   " => "3",
    "   |_|  |   " => "4",
    " _ |_  _|   " => "5",
    " _ |_ |_|   " => "6",
    " _   |  |   " => "7",
    " _ |_||_|   " => "8",
    " _ |_| _|   " => "9"
  }
  @height 4
  @width 3
  @unrecognised "?"

  @doc """
  Given a 3 x 4 grid of pipes, underscores, and spaces, determine which number is represented, or
  whether it is garbled.
  """
  @spec convert([String.t()]) :: String.t()
  def convert(input) do
    with :ok <- check_height(input),
         :ok <- check_width(input) do
      {:ok, to_number(input)}
    end
  end

  defp check_height(input) do
    if valid_height?(input) do
      :ok
    else
      {:error, 'invalid line count'}
    end
  end

  defp valid_height?(input) do
    input
    |> length()
    |> rem(@height)
    |> Kernel.==(0)
  end

  defp check_width(input) do
    if Enum.all?(input, &valid_width?/1) do
      :ok
    else
      {:error, 'invalid column count'}
    end
  end

  defp valid_width?(input) do
    input
    |> String.length()
    |> rem(@width)
    |> Kernel.==(0)
  end

  defp to_number(input) do
    input
    |> Enum.chunk_every(@height)
    |> Enum.map(&convert_row/1)
    |> Enum.join(",")
  end

  defp convert_row(row) do
    row
    |> Enum.map(&split_by_digits/1)
    |> transpose()
    |> Enum.map(&Enum.join/1)
    |> Enum.map(&convert_to_number/1)
    |> Enum.join()
  end

  defp split_by_digits(line) do
    line
    |> String.graphemes()
    |> Enum.chunk_every(@width)
    |> Enum.map(&Enum.join/1)
  end

  defp transpose(row) do
    row
    |> Enum.zip()
    |> Enum.map(&Tuple.to_list/1)
  end

  def convert_to_number(ocr), do: Map.get(@conversions, ocr, @unrecognised)
end
