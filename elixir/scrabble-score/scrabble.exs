defmodule Scrabble do
  @one_point_letters ~w(a e i o u l n r s t)
  @two_point_letters ~w(d g)
  @three_point_letters ~w(b c m p)
  @four_point_letters ~w(f h v w y)
  @five_point_letters ~w(k)
  @eight_point_letters ~w(j x)
  @ten_point_letters ~w(q z)

  defguardp one_point(letter) when letter in @one_point_letters
  defguardp two_points(letter) when letter in @two_point_letters
  defguardp three_points(letter) when letter in @three_point_letters
  defguardp four_points(letter) when letter in @four_point_letters
  defguardp five_points(letter) when letter in @five_point_letters
  defguardp eight_points(letter) when letter in @eight_point_letters
  defguardp ten_points(letter) when letter in @ten_point_letters

  @doc """
  Calculate the scrabble score for the word.
  """
  @spec score(String.t()) :: non_neg_integer
  def score(word) do
    word
    |> String.split("", trim: true)
    |> Stream.map(&String.downcase/1)
    |> Enum.reduce(0, &add_score_for_letter/2)
  end

  defp add_score_for_letter(letter, acc) when one_point(letter), do: acc + 1
  defp add_score_for_letter(letter, acc) when two_points(letter), do: acc + 2
  defp add_score_for_letter(letter, acc) when three_points(letter), do: acc + 3
  defp add_score_for_letter(letter, acc) when four_points(letter), do: acc + 4
  defp add_score_for_letter(letter, acc) when five_points(letter), do: acc + 5
  defp add_score_for_letter(letter, acc) when eight_points(letter), do: acc + 8
  defp add_score_for_letter(letter, acc) when ten_points(letter), do: acc + 10
  defp add_score_for_letter(_letter, acc), do: acc
end
