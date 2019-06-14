defmodule RNATranscription do
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
  def to_rna(dna) do
    dna
    |> Enum.map(&rna_transcription/1)
  end

  defp rna_transcription(dna) do
    @rna_transcriptions
    |> Map.fetch!(dna)
  end
end
