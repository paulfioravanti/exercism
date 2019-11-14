defmodule Diamond do
  @diamond_tip ?A
  @tip_only_diamond "A\n"

  @doc """
  Given a letter, it prints a diamond starting with 'A',
  with the supplied letter at the widest point.
  """
  @spec build_shape(char) :: String.t()
  def build_shape(@diamond_tip), do: @tip_only_diamond

  def build_shape(letter) do
    letters = generate_letters(letter)
    max_outer_padding = length_from_center(letter)
    diamond_height = length(letters)

    letters
    |> Enum.reduce([], &add_line({diamond_height, max_outer_padding}, &1, &2))
    |> Enum.reverse()
    |> Enum.join()
  end

  defp generate_letters(letter) do
    Enum.concat(@diamond_tip..letter, (letter - 1)..@diamond_tip)
  end

  defp length_from_center(letter), do: letter - @diamond_tip

  defp add_line({diamond_height, max_outer_padding}, letter, acc) do
    line =
      if letter == @diamond_tip do
        diamond_tip(letter, diamond_height)
      else
        diamond_row(letter, max_outer_padding)
      end

    [line <> "\n" | acc]
  end

  defp diamond_tip(letter, diamond_height) do
    [letter]
    |> :string.pad(diamond_height, :both)
    |> List.to_string()
  end

  defp diamond_row(letter, max_outer_padding) do
    inner_padding = length_from_center(letter)
    # Pad string spacing outwards from center first, then have outer
    # padding fill in the remaining gaps, if any.
    first_half =
      [letter]
      |> List.to_string()
      |> String.pad_trailing(inner_padding)
      |> String.pad_leading(max_outer_padding)

    first_half <> " " <> String.reverse(first_half)
  end
end
