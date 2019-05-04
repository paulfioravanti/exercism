# frozen_string_literal: true

class Nucleotide
  INCREMENT = 1
  private_constant :INCREMENT
  NUCLEOTIDE_SEEDS = { "A" => 0, "T" => 0, "C" => 0, "G" => 0 }.freeze
  private_constant :NUCLEOTIDE_SEEDS
  VALID_DNA_STRAND = /\A[#{NUCLEOTIDE_SEEDS.keys.join}]*\z/.freeze
  private_constant :VALID_DNA_STRAND

  def self.from_dna(string)
    raise ArgumentError unless string.match?(VALID_DNA_STRAND)

    new(string)
  end

  private_class_method :new

  def initialize(string)
    @nucleotides = string.chars
  end

  def count(nucleotide)
    nucleotides.count { |dna_nucleotide| dna_nucleotide == nucleotide }
  end

  def histogram
    nucleotides.each_with_object(
      NUCLEOTIDE_SEEDS.dup, &method(:increment_occurrence)
    )
  end

  private

  attr_reader :nucleotides

  def increment_occurrence(nucleotide, occurrences)
    occurrences[nucleotide] += INCREMENT
  end
end
