defmodule CollatzConjecture do
  @initial_steps 0
  @terminating_number 1

  require Integer

  defguardp positive_number?(input) when is_integer(input) and input > 0
  defguardp terminating_number?(input) when input == @terminating_number

  @doc """
  calc/1 takes an integer and returns the number of steps required to get the
  number to 1 when following the rules:
    - if number is odd, multiply with 3 and add 1
    - if number is even, divide by 2
  """
  @spec calc(input :: pos_integer()) :: non_neg_integer()
  def calc(input) when positive_number?(input) do
    do_calc(input, @initial_steps)
  end

  defp do_calc(input, steps) when terminating_number?(input), do: steps

  defp do_calc(input, steps) do
    input
    |> perform_calc()
    |> do_calc(steps + 1)
  end

  defp perform_calc(input) do
    if Integer.is_even(input) do
      n_div_two(input)
    else
      three_n_plus_one(input)
    end
  end

  defp n_div_two(input), do: div(input, 2)
  defp three_n_plus_one(input), do: 3 * input + 1
end
