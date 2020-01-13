#if !defined(PANGRAM_H)
#define PANGRAM_H

#include <string> // isalpha, tolower
#include <unordered_set> // unordered_set

namespace pangram {
  namespace detail {
    static const int NUMBER_OF_LETTERS_IN_ALPHABET = 26;
  }

  bool is_pangram(const std::string sentence);
}

#endif
