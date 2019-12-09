defmodule CollatzConjecture do
  @initial_steps 0
  @terminating_number 1

  require Integer

  defguardp positive_integer?(input) when is_integer(input) and input > 0

  @doc """
  calc/1 takes an integer and returns the number of steps required to get the
  number to 1 when following the rules:
    - if number is odd, multiply with 3 and add 1
    - if number is even, divide by 2
  """
  @spec calc(input :: pos_integer()) :: non_neg_integer()
  def calc(input) when positive_integer?(input), do: calc(input, @initial_steps)
  defp calc(@terminating_number, steps), do: steps

  defp calc(input, steps) when Integer.is_even(input) do
    input
    |> n_div_two()
    |> calc(steps + 1)
  end

  defp calc(input, steps) do
    input
    |> three_n_plus_one()
    |> calc(steps + 1)
  end

  defp n_div_two(n), do: div(n, 2)
  defp three_n_plus_one(n), do: 3 * n + 1
end
