defmodule Connect do
  @x_piece "X"
  @o_piece "O"
  @players %{
    @x_piece => :black,
    @o_piece => :white
  }
  @over_edge_index -1

  defguardp horizontal_edge?(board, col)
            when col ==
                   board
                   |> hd()
                   |> length()
                   |> Kernel.-(1)

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
    |> Enum.any?(&opposite_side_connection?(board, piece, &1))
  end

  defp opposite_side_connection?(board, piece, {[square | _rest], row_idx}) do
    piece == square && opposite_side_reached?(board, piece, [{row_idx, 0}])
  end

  defp opposite_side_reached?(board, _piece, [{_row_idx, col_idx} | _rest])
       when horizontal_edge?(board, col_idx),
       do: true

  defp opposite_side_reached?(board, piece, [{row_idx, col_idx} | _rest] = path) do
    board
    |> adjacent_squares(row_idx, col_idx)
    |> Enum.reject(&already_in_path?(path, &1))
    |> Enum.filter(&contains_piece?(board, piece, &1))
    |> Enum.any?(&opposite_side_reached?(board, piece, [&1 | path]))
  end

  defp already_in_path?(path, coordinate), do: coordinate in path

  defp contains_piece?(board, piece, {row, col}) do
    board
    |> Enum.at(row)
    |> Enum.at(col)
    |> Kernel.==(piece)
  end

  defp transpose(board) do
    board
    |> List.zip()
    |> Enum.map(&Tuple.to_list/1)
  end

  defp adjacent_squares(board, row_index, col_index) do
    for y <- adjacent_range(row_index),
        x <- adjacent_range(col_index),
        y > @over_edge_index,
        x > @over_edge_index,
        y < length(board),
        x <= horizontal_edge(board),
        {y, x} != {row_index, col_index} do
      {y, x}
    end
  end

  defp adjacent_range(index), do: (index - 1)..(index + 1)

  defp horizontal_edge(board) do
    board
    |> hd()
    |> length()
    |> Kernel.-(1)
  end
end
