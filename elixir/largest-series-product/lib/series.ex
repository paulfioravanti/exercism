defmodule Series do
  defguardp span_too_large?(number_string, size)
            when size > byte_size(number_string)

  defguardp non_zero_size_on_empty_string?(number_string, size)
            when size != 0 and number_string == ""

  defguardp negative_size_on_non_empty_string?(number_string, size)
            when size < 0 and number_string != ""

  defguardp invalid?(number_string, size)
            when span_too_large?(number_string, size) or
                   non_zero_size_on_empty_string?(number_string, size) or
                   negative_size_on_non_empty_string?(number_string, size)

  @zero_series_product 1

  @doc """
  Finds the largest product of a given number of consecutive numbers in a given string of numbers.
  """
  @spec largest_product(String.t(), non_neg_integer) :: non_neg_integer
  def largest_product(number_string, size) when invalid?(number_string, size) do
    raise ArgumentError
  end

  def largest_product(_number_string, 0), do: @zero_series_product

  def largest_product(number_string, size) do
    number_string
    |> String.graphemes()
    |> Enum.map(&String.to_integer/1)
    |> Enum.chunk_every(size, 1, :discard)
    |> Enum.reduce(0, &compare_products/2)
  end

  defp compare_products(subseries, acc) do
    product = Enum.reduce(subseries, &*/2)
    if product > acc, do: product, else: acc
  end
end
