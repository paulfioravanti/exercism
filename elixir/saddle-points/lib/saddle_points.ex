defmodule SaddlePoints do
  @doc """
  Parses a string representation of a matrix
  to a list of rows
  """
  @spec rows(String.t()) :: [[integer]]
  def rows(str) do
    str
    |> String.split("\n")
    |> Enum.map(&row_to_integers/1)
  end

  @doc """
  Parses a string representation of a matrix
  to a list of columns
  """
  @spec columns(String.t()) :: [[integer]]
  def columns(str) do
    str
    |> rows()
    |> List.zip()
    |> Enum.map(&Tuple.to_list/1)
  end

  @doc """
  Calculates all the saddle points from a string
  representation of a matrix
  """
  @spec saddle_points(String.t()) :: [{integer, integer}]
  def saddle_points(str) do
    columns = columns(str)

    str
    |> rows()
    |> Enum.with_index()
    |> Enum.reduce([], &check_row_saddle_points(columns, &1, &2))
  end

  defp row_to_integers(row) do
    row
    |> String.split()
    |> Enum.map(&String.to_integer/1)
  end

  defp check_row_saddle_points(columns, {row, row_index}, acc) do
    acc = {row, row_index, acc}

    row
    |> Enum.with_index()
    |> Enum.reduce(acc, &check_column_saddle_points(columns, &1, &2))
    |> elem(2)
    |> Enum.sort()
  end

  defp check_column_saddle_points(columns, {value, column_index}, acc) do
    {row, row_index, acc} = acc
    column = Enum.at(columns, column_index)

    if saddle_point?(value, row, column) do
      acc = [{row_index, column_index} | acc]
      {row, row_index, acc}
    else
      {row, row_index, acc}
    end
  end

  defp saddle_point?(value, row, column) do
    value == Enum.max(row) and value == Enum.min(column)
  end
end
