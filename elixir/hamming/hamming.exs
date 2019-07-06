defmodule Hamming do
  defguardp same_length?(strand1, strand2)
            when length(strand1) == length(strand2)

  @doc """
  Returns number of differences between two strands of DNA, known as the Hamming
  Distance.

  ## Examples

  iex> Hamming.hamming_distance('AAGTCATA', 'TAGCGATC')
  {:ok, 4}
  """
  @spec hamming_distance([char], [char]) ::
          {:ok, non_neg_integer} | {:error, String.t()}
  def hamming_distance(strand, strand), do: {:ok, 0}

  def hamming_distance(strand1, strand2) when same_length?(strand1, strand2) do
    0..length(strand1)
    |> Enum.count(&difference?(strand1, strand2, &1))
    |> (&{:ok, &1}).()
  end

  def hamming_distance(_strand1, _strand2) do
    {:error, "Lists must be the same length"}
  end

  defp difference?(strand1, strand2, nucleotide) do
    Enum.at(strand1, nucleotide) != Enum.at(strand2, nucleotide)
  end
end
