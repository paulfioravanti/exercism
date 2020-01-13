struct RnaTranscription {
  enum TranscriptionError: Error {
    case invalidNucleotide(String)
  }
}

struct Nucleotide {
  private enum Dna: Character {
    case C = "C"
    case G = "G"
    case A = "A"
    case T = "T"
  }

  private enum Rna: Character {
    case G = "G"
    case C = "C"
    case U = "U"
    case A = "A"
  }

  private static let RNA_TRANSCRIPTIONS: [Dna: Rna] = [
    Dna.C: Rna.G,
    Dna.G: Rna.C,
    Dna.A: Rna.U,
    Dna.T: Rna.A
  ]

  private let dnaStrand : String

  init(_ dnaStrand: String) {
    self.dnaStrand = dnaStrand
  }

  func complementOfDNA() throws -> String {
    return try String(dnaStrand.map(transcribe))
  }

  private func transcribe (_ nucleotide: Character) throws -> Character {
    guard
      let dna = Dna(rawValue: nucleotide),
      let rna = Nucleotide.RNA_TRANSCRIPTIONS[dna]
    else {
      throw RnaTranscription.TranscriptionError.invalidNucleotide(
        "\(nucleotide) is not a valid Nucleotide"
      )
    }
    return rna.rawValue
  }
}
