defmodule ArmstrongNumber do
  @moduledoc """
  Provides a way to validate whether or not a number is an Armstrong number
  """

  @spec valid?(integer) :: boolean
  def valid?(number) do
    sum_of_powers =
      number
      |> Integer.digits()
      |> sum_powers()

    number == sum_of_powers
  end

  defp sum_powers(digits) do
    length = length(digits)
    Enum.reduce(digits, 0, &power(length, &1, &2))
  end

  defp power(length, digit, acc) do
    acc + :math.pow(digit, length)
  end
end
