defmodule RailFenceCipher do
  @doc """
  Encode a given plaintext to the corresponding rail fence ciphertext
  """
  @spec encode(String.t(), pos_integer) :: String.t()
  def encode("", _rails), do: ""
  def encode(str, 1), do: str

  def encode(str, rails) do
    letters = String.graphemes(str)

    rails
    |> generate_fence()
    |> Enum.zip(letters)
    |> Enum.sort_by(&rail/1)
    |> Enum.map_join(&character/1)
  end

  @doc """
  Decode a given rail fence ciphertext to the corresponding plaintext
  """
  @spec decode(String.t(), pos_integer) :: String.t()
  def decode("", _rails), do: ""
  def decode(str, 1), do: str

  def decode(str, rails) do
    letters = String.graphemes(str)
    length = length(letters)

    rails
    |> generate_fence()
    |> Enum.take(length)
    |> Enum.with_index()
    |> Enum.sort()
    |> Enum.zip(letters)
    |> Enum.sort_by(&column_index/1)
    |> Enum.map_join(&character/1)
  end

  defp generate_fence(rails) do
    zig = 0..(rails - 1)
    zag = (rails - 2)..1

    zig
    |> Enum.concat(zag)
    |> Stream.cycle()
  end

  defp rail({rail, _char}), do: rail
  defp character({_rail_info, char}), do: char
  defp column_index({{_rail, index}, _char}), do: index
end
