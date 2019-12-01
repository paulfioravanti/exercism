defmodule Spiral do
  @terminal_column_index 0
  @start_number 1

  @doc """
  Given the dimension, return a square matrix of numbers in clockwise spiral order.
  """
  @spec matrix(dimension :: integer) :: list(list(integer))
  def matrix(@terminal_column_index), do: []
  def matrix(dimension), do: spiral(dimension, dimension, @start_number)

  defp spiral(_row_index, @terminal_column_index, _number), do: []

  defp spiral(row_index, column_index, start_number) do
    head_row = generate_row(start_number, column_index)
    next_row_index = column_index
    next_column_index = row_index - 1
    next_start_number = start_number + column_index

    tail_rows =
      spiral(next_row_index, next_column_index, next_start_number)
      |> rotate_right()

    [head_row | tail_rows]
  end

  defp generate_row(start_number, column_index) do
    terminal_column_index = start_number + column_index - 1

    start_number..terminal_column_index
    |> Enum.to_list()
  end

  defp rotate_right(matrix) do
    matrix
    |> Enum.reverse()
    |> transpose()
  end

  defp transpose(matrix) do
    matrix
    |> List.zip()
    |> Enum.map(&Tuple.to_list/1)
  end
end
