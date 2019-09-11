defmodule Queens do
  @board_length 8
  @black "B"
  @white "W"
  @blank "_"

  @type t :: %Queens{black: {integer, integer}, white: {integer, integer}}
  defstruct black: nil, white: nil

  @doc """
  Creates a new set of Queens
  """
  @spec new() :: Queens.t()
  @spec new({integer, integer}, {integer, integer}) :: Queens.t()
  def new(white \\ {0, 3}, black \\ {7, 3})
  def new(space, space), do: raise(ArgumentError)
  def new(white, black), do: %Queens{white: white, black: black}

  @doc """
  Gives a string representation of the board with
  white and black queen locations shown
  """
  @spec to_string(Queens.t()) :: String.t()
  def to_string(queens) do
    generate_row(&row(queens, &1, &2))
    |> Enum.map(&Enum.join(&1, " "))
    |> Enum.join("\n")
  end

  @doc """
  Checks if the queens can attack each other
  """
  @spec can_attack?(Queens.t()) :: boolean
  def can_attack?(queens) do
    %Queens{
      white: {white_row, white_col},
      black: {black_row, black_col}
    } = queens

    same_row_or_column?(white_row, white_col, black_row, black_col) or
      same_diagonal?(white_row, white_col, black_row, black_col)
  end

  defp generate_row(fun) do
    0..(@board_length - 1)
    |> Enum.reduce([], &fun.(&1, &2))
    |> Enum.reverse()
  end

  defp row(queens, row_num, acc) do
    row =
      case queens do
        %Queens{white: {^row_num, white_col}, black: {^row_num, black_col}} ->
          generate_row(&add_column(white_col, black_col, &1, &2))

        %Queens{white: {^row_num, white_col}} ->
          generate_row(&add_column(white_col, nil, &1, &2))

        %Queens{black: {^row_num, black_col}} ->
          generate_row(&add_column(nil, black_col, &1, &2))

        _ ->
          List.duplicate("_", @board_length)
      end

    [row | acc]
  end

  defp add_column(white_col, _black_col, white_col, acc), do: [@white | acc]
  defp add_column(_white_col, black_col, black_col, acc), do: [@black | acc]
  defp add_column(_white_col, _black_col, _column, acc), do: [@blank | acc]

  defp same_row_or_column?(white_row, white_col, black_row, black_col) do
    white_row == black_row or white_col == black_col
  end

  defp same_diagonal?(white_row, white_col, black_row, black_col) do
    [{white_row, black_row}, {white_col, black_col}]
    |> Enum.map(&position_abs/1)
    |> Enum.uniq()
    |> length() == 1
  end

  defp position_abs({position1, position2}), do: abs(position1 - position2)
end
