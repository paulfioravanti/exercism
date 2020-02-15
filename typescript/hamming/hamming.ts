type MaybeNumber = number | never
type NucleotideFilter = (leftNucleotide: string, index: number) => boolean

export default class Hamming {
  compute(leftStrand: string, rightStrand: string): MaybeNumber {
    if (leftStrand === rightStrand) {
      return 0
    }
    if (leftStrand.length !== rightStrand.length) {
      throw new Error("DNA strands must be of equal length.")
    }

    return (
      leftStrand
        .split("")
        .filter(this.isDifferent(rightStrand))
        .length
    )
  }

  private isDifferent(rightStrand: string): NucleotideFilter {
    return (leftNucleotide: string, index: number): boolean => {
      return leftNucleotide !== rightStrand.charAt(index)
    }
  }
}
