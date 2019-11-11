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
    for {row, row_index} <- indexed(:rows, str),
        {column, column_index} <- indexed(:columns, str),
        saddle_point?(row, column) do
      {row_index, column_index}
    end
  end

  defp indexed(fun, str) do
    SaddlePoints
    |> apply(fun, [str])
    |> Enum.with_index()
  end

  defp row_to_integers(row) do
    row
    |> String.split()
    |> Enum.map(&String.to_integer/1)
  end

  defp saddle_point?(row, column), do: Enum.max(row) == Enum.min(column)
end
