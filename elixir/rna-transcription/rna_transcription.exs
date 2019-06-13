defmodule RNATranscription do
  @rna_transcriptions %{
    "A" => 'U',
    "C" => 'G',
    "G" => 'C',
    "T" => 'A'
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
    |> to_string()
    |> String.split("", trim: true)
    |> Enum.reduce('', &append_rna_transcription/2)
  end

  defp append_rna_transcription(dna, acc) do
    @rna_transcriptions
    |> Map.fetch!(dna)
    |> (fn dna -> acc ++ dna end).()
  end
end
