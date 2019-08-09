defmodule AllYourBase do
  @minimum_base 2

  defguardp invalid_bases?(base_a, base_b)
            when base_a < @minimum_base or base_b < @minimum_base

  @doc """
  Given a number in base a, represented as a sequence of digits, converts it to
  base b, or returns nil if either of the bases are less than 2
  """
  @spec convert(list, integer, integer) :: list
  def convert([], _base_a, _base_b), do: nil

  def convert(_list, base_a, base_b) when invalid_bases?(base_a, base_b) do
    nil
  end

  def convert(digits, base_a, base_b) do
    if Enum.any?(digits, &invalid_digit?(&1, base_a)) do
      nil
    else
      digits
      |> sum_input(base_a)
      |> convert_to_output_base(base_b)
    end
  end

  defp invalid_digit?(digit, base_a) do
    digit < 0 or digit >= base_a
  end

  defp sum_input(digits, base_a) do
    digits
    |> Enum.reverse()
    |> Enum.with_index()
    |> Enum.reduce(0, &add_power(&1, &2, base_a))
  end

  defp add_power({digit, index}, acc, base_a) do
    power =
      base_a
      |> :math.pow(index)
      |> floor()

    acc + digit * power
  end

  defp convert_to_output_base(total, base_b, digits \\ []) do
    remainder = rem(total, base_b)
    digits = [remainder | digits]

    if total < base_b do
      digits
    else
      total
      |> div(base_b)
      |> convert_to_output_base(base_b, digits)
    end
  end
end
