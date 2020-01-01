#include "pangram.h"

const int NUMBER_OF_LETTERS_IN_ALPHABET = 26;

bool is_empty(const char *sentence) {
  return sentence[0] == '\0';
}

void parse_lower(char *sentence) {
  int count = 0;

  for (int i = 0; sentence[i]; i++) {
    if (isalpha(sentence[i])) {
      sentence[count++] = tolower(sentence[i]);
    }
  }

  sentence[count] = '\0';
}

int count_unique_characters(char* sentence) {
  int count = 0;

  for (size_t i = 0, len = strlen(sentence); i < len; i++) {
    bool appears = false;

    for (size_t j = 0; j < i; j++) {
      if (sentence[i] == sentence[j]) {
        appears = true;
        break;
      }
    }

    if (!appears) {
      count++;
    }
  }

  return count;
}

bool is_pangram(const char *sentence) {
  if (sentence == NULL || is_empty(sentence)) {
    return false;
  }

  char pangram[strlen(sentence)];
  strcpy(pangram, sentence);
  parse_lower(pangram);
  return count_unique_characters(pangram) == NUMBER_OF_LETTERS_IN_ALPHABET;
}
