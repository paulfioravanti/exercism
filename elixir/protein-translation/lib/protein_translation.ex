defmodule ProteinTranslation do
  @methionine "AUG"
  @phenylalanine ["UUC", "UUU"]
  @leucine ["UUA", "UUG"]
  @serine ["UCU", "UCC", "UCA", "UCG"]
  @tyrosine ["UAU", "UAC"]
  @cysteine ["UGU", "UGC"]
  @tryptophan "UGG"
  @terminating ["UAA", "UAG", "UGA"]
  @stop "STOP"
  @codon_length ~r/.{3}/

  defguardp methionine?(codon) when codon == @methionine
  defguardp phenylalanine?(codon) when codon in @phenylalanine
  defguardp leucine?(codon) when codon in @leucine
  defguardp serine?(codon) when codon in @serine
  defguardp tyrosine?(codon) when codon in @tyrosine
  defguardp cysteine?(codon) when codon in @cysteine
  defguardp tryptophan?(codon) when codon == @tryptophan
  defguardp terminating?(codon) when codon in @terminating

  @doc """
  Given an RNA string, return a list of proteins specified by codons, in order.
  """
  @spec of_rna(String.t()) :: {atom, list(String.t())}
  def of_rna(rna) do
    proteins =
      rna
      |> String.split(@codon_length, include_captures: true, trim: true)
      |> Enum.reduce([], &translate_codon/2)
      |> Enum.reverse()

    {:ok, proteins}
  catch
    {:stop, proteins} ->
      {:ok, proteins}

    :error ->
      {:error, "invalid RNA"}
  end

  @doc """
  Given a codon, return the corresponding protein

  UGU -> Cysteine
  UGC -> Cysteine
  UUA -> Leucine
  UUG -> Leucine
  AUG -> Methionine
  UUU -> Phenylalanine
  UUC -> Phenylalanine
  UCU -> Serine
  UCC -> Serine
  UCA -> Serine
  UCG -> Serine
  UGG -> Tryptophan
  UAU -> Tyrosine
  UAC -> Tyrosine
  UAA -> STOP
  UAG -> STOP
  UGA -> STOP
  """
  @spec of_codon(String.t()) :: {atom, String.t()}
  def of_codon(codon) when methionine?(codon), do: {:ok, "Methionine"}
  def of_codon(codon) when phenylalanine?(codon), do: {:ok, "Phenylalanine"}
  def of_codon(codon) when leucine?(codon), do: {:ok, "Leucine"}
  def of_codon(codon) when serine?(codon), do: {:ok, "Serine"}
  def of_codon(codon) when tyrosine?(codon), do: {:ok, "Tyrosine"}
  def of_codon(codon) when cysteine?(codon), do: {:ok, "Cysteine"}
  def of_codon(codon) when tryptophan?(codon), do: {:ok, "Tryptophan"}
  def of_codon(codon) when terminating?(codon), do: {:ok, @stop}
  def of_codon(_codon), do: {:error, "invalid codon"}

  defp translate_codon(codon, acc) do
    codon
    |> of_codon()
    |> apply_protein(acc)
  end

  defp apply_protein({:ok, @stop}, acc), do: throw({:stop, Enum.reverse(acc)})
  defp apply_protein({:ok, protein}, acc), do: [protein | acc]
  defp apply_protein({:error, _message}, _acc), do: throw(:error)
end
