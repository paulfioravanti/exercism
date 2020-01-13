#include "rna_transcription.h"

using namespace std;

namespace rna_transcription {
  char to_rna(const char dna) {
    using namespace detail;

    return RNA_TRANSCRIPTIONS[static_cast<Dna::Nucleotide>(dna)];
  }

  const string to_rna(const string dna_strand) {
    string rna_strand;
    for (const char dna : dna_strand) {
      rna_strand += to_rna(dna);
    }
    return rna_strand;
  }
}
