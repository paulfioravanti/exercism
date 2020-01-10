module RnaComplement
  extend self

  enum Dna
    G
    C
    T
    A
  end

  enum Rna
    C
    G
    A
    U
  end

  private RNA_TRANSCRIPTIONS = {
    Dna::G => Rna::C,
    Dna::C => Rna::G,
    Dna::T => Rna::A,
    Dna::A => Rna::U,
  }

  def of_dna(strand : String) : String
    strand
      .split("")
      .reduce("", &->translate_nucleotide(String, String))
  end

  private def translate_nucleotide(acc : String, nucleotide : String) : String
    acc + RNA_TRANSCRIPTIONS[Dna.parse(nucleotide)].to_s
  end
end
