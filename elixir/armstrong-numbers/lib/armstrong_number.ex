defmodule ArmstrongNumber do
  @moduledoc """
  Provides a way to validate whether or not a number is an Armstrong number
  """

  @spec valid?(integer) :: boolean
  def valid?(number) do
    sum =
      number
      |> Integer.digits()
      |> sum_powers()

    sum == number
  end

  defp sum_powers(digits) do
    Enum.reduce(digits, 0, &power(&1, &2, length(digits)))
  end

  defp power(digit, acc, length) do
    acc + :math.pow(digit, length)
  end
end
