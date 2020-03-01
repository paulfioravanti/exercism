type Methionine = "AUG"
type Phenylalanine = "UUC" | "UUU"
type Leucine = "UUA" | "UUG"
type Serine = "UCU" | "UCC" | "UCA" | "UCG"
type Tyrosine = "UAU" | "UAC"
type Cysteine = "UGU" | "UGC"
type Tryptophan = "UGG"
type Terminating = "UAA" | "UAG" | "UGA"
type Codon =
  Methionine
  | Phenylalanine
  | Leucine
  | Serine
  | Tyrosine
  | Cysteine
  | Tryptophan
  | Terminating
type Stop = "STOP"
type Translation =
  "Methionine"
  | "Phenylalanine"
  | "Leucine"
  | "Serine"
  | "Tyrosine"
  | "Cysteine"
  | "Tryptophan"
  | Stop
type MaybeError<T> = T | never

export default class ProteinTranslation {
  private static readonly METHIONINE: Methionine = "AUG"
  private static readonly PHENYLALANINE: Readonly<Phenylalanine[]> =
    Object.freeze(["UUC", "UUU"])
  private static readonly LEUCINE: Readonly<Leucine[]> =
    Object.freeze(["UUA", "UUG"])
  private static readonly SERINE: Readonly<Serine[]> =
    Object.freeze(["UCU", "UCC", "UCA", "UCG"])
  private static readonly TYROSINE: Readonly<Tyrosine[]> =
    Object.freeze(["UAU", "UAC"])
  private static readonly CYSTEINE: Readonly<Cysteine[]> =
    Object.freeze(["UGU", "UGC"])
  private static readonly TRYPTOPHAN: Tryptophan = "UGG"
  private static readonly TERMINATING: Readonly<Terminating[]> =
    Object.freeze(["UAA", "UAG", "UGA"])
  private static readonly STOP: Stop = "STOP"
  private static readonly CODON: RegExp = /(.{3})/

  static proteins(rna: string): MaybeError<Translation[]> {
    try {
      return (
        rna
          .split(ProteinTranslation.CODON)
          .filter(Boolean)
          .reduce(ProteinTranslation.translateCodon, [])
      )
    } catch (retval) {
      if (retval instanceof Error) {
        throw retval
      }
      return retval
    }
  }

  private static translateCodon(
    acc: Translation[],
    codon: string
  ): Translation[] {
    const protein: Translation =
      ProteinTranslation.proteinForCodon(codon as Codon)

    if (protein === ProteinTranslation.STOP) {
      throw acc
    }

    acc.push(protein)
    return acc
  }

  private static proteinForCodon(codon: Codon): Translation {
    if (codon === ProteinTranslation.METHIONINE) {
      return "Methionine"
    } else if (
      ProteinTranslation.PHENYLALANINE.includes(codon as Phenylalanine)
    ) {
      return "Phenylalanine"
    } else if (ProteinTranslation.LEUCINE.includes(codon as Leucine)) {
      return "Leucine"
    } else if (ProteinTranslation.SERINE.includes(codon as Serine)) {
      return "Serine"
    } else if (ProteinTranslation.TYROSINE.includes(codon as Tyrosine)) {
      return "Tyrosine"
    } else if (ProteinTranslation.CYSTEINE.includes(codon as Cysteine)) {
      return "Cysteine"
    } else if (codon === ProteinTranslation.TRYPTOPHAN) {
      return "Tryptophan"
    } else {
      return ProteinTranslation.STOP
    }
  }
}
