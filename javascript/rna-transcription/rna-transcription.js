const DNA = Object.freeze({
  C: 'C',
  G: 'G',
  A: 'A',
  T: 'T'
})

const RNA = Object.freeze({
  G: 'G',
  C: 'C',
  U: 'U',
  A: 'A'
})

const RNA_TRANSCRIPTIONS = Object.freeze({
  [DNA.C]: RNA.G,
  [DNA.G]: RNA.C,
  [DNA.A]: RNA.U,
  [DNA.T]: RNA.A
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
