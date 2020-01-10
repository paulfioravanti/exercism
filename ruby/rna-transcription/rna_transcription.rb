# frozen_string_literal: true

module Complement
  module Dna
    A = "A"
    C = "C"
    G = "G"
    T = "T"
  end
  private_constant :Dna

  module Rna
    U = "U"
    G = "G"
    C = "C"
    A = "A"
  end
  private_constant :Rna

  RNA_TRANSCRIPTIONS = {
    Dna::A => Rna::U,
    Dna::C => Rna::G,
    Dna::G => Rna::C,
    Dna::T => Rna::A
  }.freeze
  private_constant :RNA_TRANSCRIPTIONS
  DNA_STRAND = /\A[#{RNA_TRANSCRIPTIONS.keys.join}]+\z/.freeze
  private_constant :DNA_STRAND

  module_function

  def of_dna(string)
    return "" unless string.match?(DNA_STRAND)

    string.chars.reduce("") do |rna_strand, dna_nucleotide|
      rna_strand + RNA_TRANSCRIPTIONS[dna_nucleotide]
    end
  end
end
