module Complement
  RNA_TRANSCRIPTIONS = {
    "A" => "U",
    "C" => "G",
    "G" => "C",
    "T" => "A"
  }.freeze
  private_constant :RNA_TRANSCRIPTIONS
  DNA_STRAND = /\A[#{RNA_TRANSCRIPTIONS.keys.join}]+\z/
  private_constant :DNA_STRAND

  module_function

  def of_dna(string)
    return "" unless string.match?(DNA_STRAND)
    string.chars.reduce("") do |rna_strand, dna_nucleotide|
      rna_strand << RNA_TRANSCRIPTIONS[dna_nucleotide]
    end
  end
end

module BookKeeping
  VERSION = 4
end
