defmodule NucleotideCount do
  @nucleotides [?A, ?C, ?G, ?T]

  @doc """
  Counts individual nucleotides in a NucleotideCount strand.

  ## Examples

  iex> NucleotideCount.count('AATAA', ?A)
  4

  iex> NucleotideCount.count('AATAA', ?T)
  1
  """
  @spec count([char], char) :: non_neg_integer
  def count(strand, nucleotide) do
    Enum.reduce(strand, 0, &increment_nucleotide(nucleotide, &1, &2))
  end

  @doc """
  Returns a summary of counts by nucleotide.

  ## Examples

  iex> NucleotideCount.histogram('AATAA')
  %{?A => 4, ?T => 1, ?C => 0, ?G => 0}
  """
  @spec histogram([char]) :: map
  def histogram(strand) do
    Enum.reduce(strand, initial_nucleotides(), &increment_histogram/2)
  end

  defp increment_nucleotide(nucleotide, nucleotide, acc), do: acc + 1
  defp increment_nucleotide(_nucleotide, _char, acc), do: acc
  defp increment_histogram(char, acc), do: Map.update(acc, char, 1, &(&1 + 1))

  defp initial_nucleotides do
    Enum.reduce(@nucleotides, %{}, &Map.put(&2, &1, 0))
  end
end
