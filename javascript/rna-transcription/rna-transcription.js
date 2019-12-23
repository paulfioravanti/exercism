const RNA_TRANSCRIPTIONS = Object.freeze({
  C: "G",
  G: "C",
  A: "U",
  T: "A"
})

export const toRna = dna => {
  return dna
    .split("")
    .map(translateNucleotide)
    .join("")
}

const translateNucleotide = nucleotide => {
  return RNA_TRANSCRIPTIONS[nucleotide]
}
