defmodule Minesweeper do
  @mine "*"
  @beginning_outer_bound -1

  @doc """
  Annotate empty spots next to mines with the number of mines next to them.
  """
  @spec annotate([String.t()]) :: [String.t()]

  def annotate([]), do: []

  def annotate(board) do
    board
    |> Enum.with_index()
    |> Enum.map(&annotate_line(board, &1))
  end

  defp annotate_line(board, {line, y_index}) do
    line
    |> String.codepoints()
    |> Enum.with_index()
    |> Enum.map(&annotate_character({y_index, board}, &1))
    |> Enum.join()
  end

  defp annotate_character({y_index, board}, {char, x_index}) do
    if char == @mine do
      char
    else
      board
      |> adjacent_coordinates(y_index, x_index)
      |> Enum.reduce(0, &sum_mine(board, &1, &2))
      |> substitute_character(char)
    end
  end

  defp adjacent_coordinates(board, y_index, x_index) do
    {left, right, above, below} = get_adjacent_coordinates(y_index, x_index)

    [{y_index, left}, {y_index, right}]
    |> concat_above_coordinates(board, {x_index, left, right, above})
    |> concat_below_coordinates(board, {x_index, left, right, below})
  end

  defp get_adjacent_coordinates(y_index, x_index) do
    {x_index - 1, x_index + 1, y_index - 1, y_index + 1}
  end

  defp concat_above_coordinates(
         coordinates,
         board,
         {x_index, left, right, above}
       ) do
    if above > @beginning_outer_bound and Enum.at(board, above) do
      [{above, x_index}, {above, right}]
      |> concat_left_coordinate(above, left)
      |> Enum.concat(coordinates)
    else
      coordinates
    end
  end

  defp concat_below_coordinates(
         coordinates,
         board,
         {x_index, left, right, below}
       ) do
    if Enum.at(board, below) do
      [{below, x_index}, {below, right}]
      |> concat_left_coordinate(below, left)
      |> Enum.concat(coordinates)
    else
      coordinates
    end
  end

  defp concat_left_coordinate(coordinates, y_direction, left) do
    if left > @beginning_outer_bound do
      [{y_direction, left} | coordinates]
    else
      coordinates
    end
  end

  defp sum_mine(board, {y, x}, acc) do
    char =
      board
      |> Enum.at(y)
      |> String.codepoints()
      |> Enum.at(x)

    if char == @mine, do: acc + 1, else: acc
  end

  defp substitute_character(sum, char), do: if(sum > 0, do: sum, else: char)
end
