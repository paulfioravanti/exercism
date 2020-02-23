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

type MaybeError<T> = T | never
type RnaTranscriptions = Readonly<Record<Dna, Rna>>

export default class Transcriptor {
  private readonly RNA_TRANSCRIPTIONS: RnaTranscriptions = {
    [Dna.C]: Rna.G,
    [Dna.G]: Rna.C,
    [Dna.A]: Rna.U,
    [Dna.T]: Rna.A
  }

  toRna(dna: string): string {
    return (
      dna
        .split("")
        .map(this.translateNucleotide.bind(this))
        .join("")
    )
  }

  private translateNucleotide(nucleotide: string): MaybeError<Rna> {
    return this.RNA_TRANSCRIPTIONS[nucleotide as Dna] || this.throwInvalidDna()
  }

  private throwInvalidDna(): never {
    throw new Error("Invalid input DNA.")
  }
}
