#ifndef PANGRAM_H
#define PANGRAM_H

#include <ctype.h> // tolower, isalpha
#include <stdbool.h> // bool
#include <string.h> // NULL, strdup, strlen

const int NUMBER_OF_LETTERS_IN_ALPHABET;

void parse_lower(char *str);

int count_unique_characters(char* str);

bool is_pangram(const char *sentence);

#endif
