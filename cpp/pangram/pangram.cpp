#include "pangram.h"

using namespace std;

namespace pangram {
  bool is_pangram(const string sentence) {
    unordered_set<char> letters;

    for (const char character : sentence) {
      if (isalpha(character)) {
        letters.insert(tolower(character));
      }
    }

    return letters.size() == NUMBER_OF_LETTERS_IN_ALPHABET;
  }
}
