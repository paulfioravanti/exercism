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
    Enum.reduce(strand, 0, fn(char, count) ->
      case char do
        ^nucleotide -> count + 1
        _ -> count
      end
    end)
  end


  @doc """
  Returns a summary of counts by nucleotide.

  ## Examples

  iex> NucleotideCount.histogram('AATAA')
  %{?A => 4, ?T => 1, ?C => 0, ?G => 0}
  """
  @spec histogram([char]) :: map
  def histogram(strand) do
    Enum.reduce(strand, initial_nucleotides(), fn(char, histogram) ->
      Map.update(histogram, char, 1, fn(value) ->
        value + 1
      end)
    end)
  end

  defp initial_nucleotides do
    Enum.reduce(@nucleotides, %{}, fn(elem, map) ->
      Map.put(map, elem, 0)
    end)
  end
end