# frozen_string_literal: true

class InvalidCodonError < StandardError; end

module Translation
  CODONS = lambda do |nucleotide_sequence|
    case nucleotide_sequence
    when "AUG"
      "Methionine"
    when "UUC", "UUU"
      "Phenylalanine"
    when "UUA", "UUG"
      "Leucine"
    when "UCU", "UCC", "UCA", "UCG"
      "Serine"
    when "UAU", "UAC"
      "Tyrosine"
    when "UGU", "UGC"
      "Cysteine"
    when "UGG"
      "Tryptophan"
    when "UAA", "UAG", "UGA"
      TERMINATING_CODON
    else
      raise InvalidCodonError
    end
  end
  private_constant :CODONS
  CODON_LENGTH = /.{3}/.freeze
  private_constant :CODON_LENGTH
  TERMINATING_CODON = "STOP"
  private_constant :TERMINATING_CODON

  module_function

  def of_codon(codon)
    CODONS[codon]
  end

  def of_rna(rna)
    catch(:halt) do
      rna
        .scan(CODON_LENGTH)
        .reduce([], &method(:translate_codon))
    end
  end

  def translate_codon(acc, sequence)
    codon = of_codon(sequence)
    throw(:halt, acc) if codon == TERMINATING_CODON

    acc << codon
  end
  private_class_method :translate_codon
end
