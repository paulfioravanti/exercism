defmodule Palindromes do
  @doc """
  Generates all palindrome products from an optionally given min factor (or 1) to a given max factor.
  """
  @spec generate(non_neg_integer, non_neg_integer) :: map
  def generate(max_factor, min_factor \\ 1) do
    min_factor..max_factor
    |> generate_palindrome_product_factors()
    |> Enum.reduce(%{}, &compile_product_factors/2)
  end

  defp generate_palindrome_product_factors(range) do
    for x <- range,
        y <- range,
        x <= y,
        product = x * y,
        digits = Integer.digits(product),
        palindrome?(digits) do
      {product, [x, y]}
    end
  end

  defp palindrome?(digits), do: digits == Enum.reverse(digits)

  defp compile_product_factors({product, factors}, acc) do
    Map.update(acc, product, [factors], &[factors | &1])
  end
end
