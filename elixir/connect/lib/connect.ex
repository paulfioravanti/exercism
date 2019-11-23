defmodule Connect do
  @x_piece "X"
  @o_piece "O"
  @players %{
    @x_piece => :black,
    @o_piece => :white
  }
  @out_of_bounds_index -1

  @doc """
  Calculates the winner (if any) of a board
  using "O" as the white player
  and "X" as the black player
  """
  @spec result_for([String.t()]) :: :none | :black | :white
  def result_for([piece]), do: @players[piece]

  def result_for(board) do
    board = Enum.map(board, &String.graphemes/1)

    cond do
      winner?(transpose(board), @o_piece) ->
        @players[@o_piece]

      winner?(board, @x_piece) ->
        @players[@x_piece]

      true ->
        :none
    end
  end

  defp winner?(board, piece) do
    board
    |> Enum.with_index()
    |> Enum.any?(&opposite_side_connected?(board, piece, &1))
  end

  defp opposite_side_connected?(board, piece, {[square | _rest], row_index}) do
    piece == square and opposite_side_reached?(board, piece, [{row_index, 0}])
  end

  defp opposite_side_reached?(board, piece, [square | _rest] = path) do
    {row_index, column_index} = square

    if column_index == opposite_edge(board) do
      true
    else
      board
      |> adjacent_squares(row_index, column_index)
      |> Enum.reject(&square_already_in_path?(path, &1))
      |> Enum.filter(&square_contains_piece?(board, piece, &1))
      |> Enum.any?(&opposite_side_reached?(board, piece, [&1 | path]))
    end
  end

  defp opposite_edge(board) do
    board
    |> hd()
    |> length()
    |> Kernel.-(1)
  end

  defp square_already_in_path?(path, square), do: square in path

  defp square_contains_piece?(board, piece, {row, column}) do
    board
    |> Enum.at(row)
    |> Enum.at(column)
    |> Kernel.==(piece)
  end

  defp transpose(board) do
    board
    |> List.zip()
    |> Enum.map(&Tuple.to_list/1)
  end

  defp adjacent_squares(board, row_index, column_index) do
    for y <- adjacent_range(row_index),
        x <- adjacent_range(column_index),
        y > @out_of_bounds_index and y < length(board),
        x > @out_of_bounds_index and x <= opposite_edge(board) do
      {y, x}
    end
  end

  defp adjacent_range(index), do: (index - 1)..(index + 1)
end
