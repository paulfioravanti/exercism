enum RNA_TRANSCRIPTIONS {
  A = "U",
  C = "G",
  G = "C",
  T = "A"
}

class Transcriptor {
  toRna(dna: string): string {
    const codons = [...dna]
    this.validateSequence(codons)
    const rna =
      codons.map(codon => RNA_TRANSCRIPTIONS[codon]).join("")
    return rna
  }

  private validateSequence(
    codons: string[]
  ): asserts codons is Array<keyof typeof RNA_TRANSCRIPTIONS> {
    if (!codons.every(this.isValidCodon)) {
      throw Error("Invalid input DNA.")
    }
  }

  private isValidCodon(codon: string): codon is keyof typeof RNA_TRANSCRIPTIONS {
    return codon in RNA_TRANSCRIPTIONS
  }
}

export default Transcriptor
