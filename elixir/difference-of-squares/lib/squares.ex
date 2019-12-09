defmodule Squares do
  @moduledoc """
  Calculate sum of squares, square of sum, difference between two sums from 1 to a given end number.
  """

  @square 2

  @doc """
  Calculate sum of squares from 1 to a given end number.
  """
  @spec sum_of_squares(pos_integer) :: pos_integer
  def sum_of_squares(number), do: Enum.reduce(1..number, &add_square/2)

  @doc """
  Calculate square of sum from 1 to a given end number.
  """
  @spec square_of_sum(pos_integer) :: pos_integer
  def square_of_sum(number) do
    1..number
    |> Enum.sum()
    |> :math.pow(@square)
  end

  @doc """
  Calculate difference between sum of squares and square of sum from 1 to a given end number.
  """
  @spec difference(pos_integer) :: pos_integer
  def difference(number), do: square_of_sum(number) - sum_of_squares(number)

  defp add_square(number, acc) do
    number
    |> :math.pow(@square)
    |> Kernel.+(acc)
  end
end