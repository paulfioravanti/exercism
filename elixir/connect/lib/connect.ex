defmodule Connect do
  defmodule ChildCoordinates do
    def generate(board, piece, y_index, x_index) do
      board
      |> generate_adjacent_coordinates(piece, y_index, x_index)
      |> Enum.reduce([], &add_valid_child({board, piece}, &1, &2))
    end

    defp generate_adjacent_coordinates(board, piece, y_index, x_index) do
      coordinates = adjacent_coordinates(y_index, x_index)

      y_index
      |> center_coordinates(coordinates)
      |> add_above_coordinates(board, x_index, piece, coordinates)
      |> add_below_coordinates(board, x_index, piece, coordinates)
    end

    defp adjacent_coordinates(y_index, x_index) do
      {x_index - 1, x_index + 1, y_index - 1, y_index + 1}
    end

    defp center_coordinates(y_index, {left, right, _above, _below}) do
      [{y_index, left}, {y_index, right}]
    end

    defp add_above_coordinates(acc, board, x_index, piece, coordinates) do
      {left, right, above, _below} = coordinates

      if Enum.at(board, above) do
        above_diagonal_coordinate =
          if piece == "X" do
            {above, right}
          else
            {above, left}
          end

        [above_diagonal_coordinate, {above, x_index} | acc]
      else
        acc
      end
    end

    defp add_below_coordinates(acc, board, x_index, piece, coordinates) do
      {left, right, _above, below} = coordinates

      if Enum.at(board, below) do
        below_diagonal_coordinate =
          if piece == "X" do
            {below, left}
          else
            {below, right}
          end

        [below_diagonal_coordinate, {below, x_index} | acc]
      else
        acc
      end
    end

    defp add_valid_child({board, piece}, {y, x} = coordinate, acc) do
      if y > 0 and x > 0 and expected_piece?(board, coordinate, piece) do
        [coordinate | acc]
      else
        acc
      end
    end

    defp expected_piece?(board, {y, x}, piece) do
      board
      |> Enum.at(y)
      |> Enum.at(x)
      |> Kernel.==(piece)
    end
  end

  @initial_column_index 0

  @doc """
  Calculates the winner (if any) of a board
  using "O" as the white player
  and "X" as the black player
  """
  @spec result_for([String.t()]) :: :none | :black | :white
  def result_for(board) do
    board = Enum.map(board, &String.codepoints/1)

    cond do
      win?(board, "X") ->
        :black

      win?(rotated_board(board), "O") ->
        :white

      true ->
        :none
    end
  end

  defp win?(board, piece) do
    board
    |> Enum.with_index()
    |> Enum.reduce({board, piece}, &check_row/2)
  catch
    {:halt, result} ->
      result
  end

  defp check_row({row, y_index}, {board, piece}) do
    if Enum.at(row, 0) == piece do
      initial_stack = [{y_index, @initial_column_index}]
      find_path(initial_stack, {board, row, piece})
    else
      throw({:halt, false})
    end
  catch
    {:break, acc} ->
      acc
  end

  defp find_path(stack, {board, row, piece}) do
    [{y_index, x_index} = first | tail] = stack

    if y_index == nil do
      throw({:halt, false})
    end

    if x_index == length(row) - 1 do
      throw({:halt, true})
    end

    children = ChildCoordinates.generate(board, piece, y_index, x_index)

    case tail do
      [] ->
        throw({:break, {board, piece}})

      _list when first == hd(tail) ->
        throw({:break, {board, piece}})

      _list ->
        children
        |> Enum.reduce(tail, &add_search_path/2)
        |> find_path({board, row, piece})
    end

    # if first == hd(tail) do
    #   throw({:break, {board, piece}})
    # else
    #   children
    #   |> Enum.reduce(tail, &add_search_path/2)
    #   |> find_path({board, row, piece})
    # end
  end

  def add_search_path(child, acc) do
    if Enum.member?(acc, child), do: acc, else: [child | acc]
  end

  defp rotated_board(board) do
    board
    |> transpose()
    |> Enum.map(&Enum.reverse/1)
  end

  defp transpose(rows) do
    rows
    |> Enum.zip()
    |> Enum.map(&Tuple.to_list/1)
  end
end
