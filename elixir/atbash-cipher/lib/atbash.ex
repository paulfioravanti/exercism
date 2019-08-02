defmodule Atbash do
  @alphabet_bounds ?a + ?z
  @group_size 5

  defguardp is_alpha?(char) when char in ?a..?z
  defguardp is_numeric?(char) when char in ?0..?9

  @doc """
  Encode a given plaintext to the corresponding ciphertext

  ## Examples

  iex> Atbash.encode("completely insecure")
  "xlnko vgvob rmhvx fiv"
  """
  @spec encode(String.t()) :: String.t()
  def encode(plaintext) do
    plaintext
    |> String.downcase()
    |> to_transposed_charlist()
    |> Enum.chunk_every(@group_size)
    |> Enum.join(" ")
  end

  @spec decode(String.t()) :: String.t()
  def decode(cipher) do
    cipher
    |> to_transposed_charlist()
    |> List.to_string()
  end

  defp to_transposed_charlist(string) do
    string
    |> String.to_charlist()
    |> List.foldr([], &transpose/2)
  end

  defp transpose(char, acc) when is_alpha?(char) do
    [@alphabet_bounds - char | acc]
  end

  defp transpose(char, acc) when is_numeric?(char), do: [char | acc]
  defp transpose(_char, acc), do: acc
end
