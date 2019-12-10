defmodule RnaTranscription do
  @rna_transcriptions %{
    ?A => ?U,
    ?C => ?G,
    ?G => ?C,
    ?T => ?A
  }

  @doc """
  Transcribes a character list representing DNA nucleotides to RNA

  ## Examples

  iex> RNATranscription.to_rna('ACTG')
  'UGAC'
  """
  @spec to_rna([char]) :: [char]
  def to_rna(dna), do: Enum.map(dna, &Map.fetch!(@rna_transcriptions, &1))
end
