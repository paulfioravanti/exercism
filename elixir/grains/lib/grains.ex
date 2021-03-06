defmodule Grains do
  @base 2
  @chessboard 1..64
  @offset 1

  defguardp valid_chessboard_square?(number) when number in @chessboard

  @doc """
  Calculate two to the power of the input minus one.
  """
  @spec square(pos_integer) :: pos_integer
  def square(number) when valid_chessboard_square?(number) do
    {:ok, calculate_square(number)}
  end

  def square(_number) do
    {:error, "The requested square must be between 1 and 64 (inclusive)"}
  end

  @doc """
  Adds square of each number from 1 to 64.
  """
  @spec total :: pos_integer
  def total do
    total_grains = Enum.reduce(@chessboard, 0, &sum_square/2)
    {:ok, total_grains}
  end

  defp sum_square(grains, acc), do: acc + calculate_square(grains)

  defp calculate_square(number) do
    @base
    |> :math.pow(number - @offset)
    |> round()
  end
end
