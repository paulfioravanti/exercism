#if !defined(RNA_TRANSCRIPTION_H)
#define RNA_TRANSCRIPTION_H

#include <map>
#include <string>

namespace rna_transcription {
  namespace detail {
    struct Dna {
      enum Nucleotide {
        G = 'G',
        C = 'C',
        A = 'A',
        T = 'T'
      };
    };

    struct Rna {
      enum Nucleotide {
        C = 'C',
        G = 'G',
        U = 'U',
        A = 'A'
      };
    };

    static std::map<Dna::Nucleotide, Rna::Nucleotide> RNA_TRANSCRIPTIONS = {
      {Dna::Nucleotide::G, Rna::Nucleotide::C},
      {Dna::Nucleotide::C, Rna::Nucleotide::G},
      {Dna::Nucleotide::A, Rna::Nucleotide::U},
      {Dna::Nucleotide::T, Rna::Nucleotide::A}
    };
  }

  char to_rna(const char);

  const std::string to_rna(const std::string);
}

#endif
