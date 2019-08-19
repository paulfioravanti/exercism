defmodule RotationalCipher do
  @alphabet_length 26

  defguardp is_lowercase?(char) when char in ?a..?z
  defguardp is_uppercase?(char) when char in ?A..?Z

  @doc """
  Given a plaintext and amount to shift by, return a rotated string.

  Example:
  iex> RotationalCipher.rotate("Attack at dawn", 13)
  "Nggnpx ng qnja"
  """
  @spec rotate(text :: String.t(), shift :: integer) :: String.t()
  def rotate(text, shift) do
    text
    |> String.to_charlist()
    |> Enum.reduce([], &rotate(shift, &1, &2))
    |> Enum.reverse()
    |> List.to_string()
  end

  defp rotate(shift, char, acc) when is_lowercase?(char) do
    [rotate_char(?a, char, shift) | acc]
  end

  defp rotate(shift, char, acc) when is_uppercase?(char) do
    [rotate_char(?A, char, shift) | acc]
  end

  defp rotate(_shift, char, acc), do: [char | acc]

  defp rotate_char(base, char, shift) do
    rem(char - base + shift, @alphabet_length) + base
  end
end
