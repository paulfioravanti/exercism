const METHIONINE = "AUG"
const PHENYLALANINE = Object.freeze(["UUC", "UUU"])
const LEUCINE = Object.freeze(["UUA", "UUG"])
const SERINE = Object.freeze(["UCU", "UCC", "UCA", "UCG"])
const TYROSINE = Object.freeze(["UAU", "UAC"])
const CYSTEINE = Object.freeze(["UGU", "UGC"])
const TRYPTOPHAN = "UGG"
const TERMINATING = Object.freeze(["UAA", "UAG", "UGA"])
const STOP = "STOP"
const CODON = /(.{3})/

export const translate = rna => {
  if (rna === undefined) {
    return []
  }

  try {
    return (
      rna
        .split(CODON)
        .filter(Boolean)
        .reduce(translateCodon, [])
    )
  } catch (retval) {
    if (retval instanceof Error) {
      throw retval
    }

    return retval
  }
}

function translateCodon(acc, codon) {
  const protein = proteinForCodon(codon)

  if (protein === STOP) {
    throw acc
  }

  acc.push(protein)
  return acc
}

function proteinForCodon(codon) {
  if (codon === METHIONINE) {
    return "Methionine"
  } else if (PHENYLALANINE.includes(codon)) {
    return "Phenylalanine"
  } else if (LEUCINE.includes(codon)) {
    return "Leucine"
  } else if (SERINE.includes(codon)) {
    return "Serine"
  } else if (TYROSINE.includes(codon)) {
    return "Tyrosine"
  } else if (CYSTEINE.includes(codon)) {
    return "Cysteine"
  } else if (codon === TRYPTOPHAN) {
    return "Tryptophan"
  } else if (TERMINATING.includes(codon)) {
    return STOP
  } else {
    throw new Error("Invalid codon")
  }
}
