#if !defined(RNA_TRANSCRIPTION_H)
#define RNA_TRANSCRIPTION_H

#include <map>
#include <string>

namespace rna_transcription {
  static std::map<char, char> RNA_TRANSCRIPTIONS = {
    {'G', 'C'},
    {'C', 'G'},
    {'A', 'U'},
    {'T', 'A'}
  };

  char to_rna(const char);

  const std::string to_rna(const std::string);
}

#endif
