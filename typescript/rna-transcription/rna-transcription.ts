const enum Dna {
  C = "C",
  G = "G",
  A = "A",
  T = "T"
}

const enum Rna {
  G = "G",
  C = "C",
  U = "U",
  A = "A"
}

type MaybeRna = Rna | never

class Transcriptor {
  private readonly RNA_TRANSCRIPTIONS: Record<Dna, Rna> = {
    [Dna.C]: Rna.G,
    [Dna.G]: Rna.C,
    [Dna.A]: Rna.U,
    [Dna.T]: Rna.A
  }

  toRna(dna: string): string {
    return dna
      .split("")
      .map(this.translateNucleotide)
      .join("")
  }

  private translateNucleotide = (nucleotide: string): MaybeRna => {
    return this.RNA_TRANSCRIPTIONS[nucleotide as Dna] || this.throwInvalidDna()
  }

  private throwInvalidDna(): never {
    throw new Error("Invalid input DNA.")
  }
}

export default Transcriptor
