type DNA = "C" | "G" | "A" | "T"
type RNA = "G" | "C" | "U" | "A"
type MaybeRNA = RNA | never

class Transcriptor {
  RNA_TRANSCRIPTIONS: {[nucleotide in DNA]: RNA} = {
    C: "G",
    G: "C",
    A: "U",
    T: "A"
  }

  toRna(dna: string): string {
    return [...dna].map(this.translateNucleotide).join("")
  }

  private translateNucleotide = (nucleotide: string): MaybeRNA => {
    return this.RNA_TRANSCRIPTIONS[nucleotide as DNA] || this.throwInvalidDna()
  }

  private throwInvalidDna(): never {
    throw new Error("Invalid input DNA.")
  }
}

export default Transcriptor
