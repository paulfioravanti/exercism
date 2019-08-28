defmodule IsbnVerifier do
  @check -1
  @isbn_10 ~r/\A\d{9}(\d|X)\z/
  @multiple 11
  @weights 10..1

  @doc """
    Checks if a string is a valid ISBN-10 identifier

    ## Examples

      iex> ISBNVerifier.isbn?("3-598-21507-X")
      true

      iex> ISBNVerifier.isbn?("3-598-2K507-0")
      false

  """
  @spec isbn?(String.t()) :: boolean
  def isbn?(isbn) do
    isbn = String.replace(isbn, "-", "")

    if String.match?(isbn, @isbn_10) do
      value =
        isbn
        |> generate_isbn_integers()
        |> Enum.zip(@weights)
        |> Enum.reduce(0, &add_isbn_formula/2)
        |> rem(@multiple)
        |> Kernel.==(0)
    else
      false
    end
  end

  defp generate_isbn_integers(isbn) do
    check =
      isbn
      |> String.at(@check)
      |> convert_check()

    isbn
    |> String.codepoints()
    |> Enum.drop(@check)
    |> Enum.map(&String.to_integer/1)
    |> Enum.concat(check)
  end

  defp convert_check("X"), do: [10]
  defp convert_check(check), do: [String.to_integer(check)]

  defp add_isbn_formula({integer, weight}, acc) do
    acc + integer * weight
  end
end
