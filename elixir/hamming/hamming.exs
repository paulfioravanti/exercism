defmodule Hamming do
  @doc """
  Returns number of differences between two strands of DNA, known as the Hamming Distance.

  ## Examples

  iex> Hamming.hamming_distance('AAGTCATA', 'TAGCGATC')
  {:ok, 4}
  """
  @spec hamming_distance([char], [char]) ::
          {:ok, non_neg_integer} | {:error, String.t()}
  def hamming_distance(strand1, strand2) do
    strand_length = length(strand1)

    cond do
      strand_length != length(strand2) ->
        {:error, "Lists must be the same length"}

      strand1 == strand2 ->
        {:ok, 0}

      true ->
        0..strand_length
        |> Enum.count(&difference?(strand1, strand2, &1))
        |> (&{:ok, &1}).()
    end
  end

  defp difference?(strand1, strand2, nucleotide) do
    Enum.at(strand1, nucleotide) != Enum.at(strand2, nucleotide)
  end
end
