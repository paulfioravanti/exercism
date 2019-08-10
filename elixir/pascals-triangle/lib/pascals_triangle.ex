defmodule PascalsTriangle do
  @doc """
  Calculates the rows of a pascal triangle
  with the given height
  """
  @spec rows(integer) :: [[integer]]
  def rows(num) do
    # https://en.wikipedia.org/wiki/Pascal%27s_triangle
    # "The rows of Pascal's triangle are conventionally enumerated
    # starting with row n = 0 at the top (the 0th row)", so instantly decrement
    # the number of rows by 1.
    0..(num - 1)
    |> Enum.map(&generate_row/1)
  end

  defp generate_row(row_num) do
    0..row_num
    |> Enum.map(&binomial(&1, row_num))
  end

  # https://en.wikipedia.org/wiki/Binomial_theorem
  # "n (row_num) choose k (exponent)" => n!/(n - k)!k!
  defp binomial(exponent, row_num) do
    factorial(row_num) / (factorial(row_num - exponent) * factorial(exponent))
  end

  defp factorial(0), do: 1

  defp factorial(num) do
    1..num
    |> Enum.reduce(1, &*/2)
  end
end
