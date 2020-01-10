#ifndef RNA_TRANSCRIPTION_H
#define RNA_TRANSCRIPTION_H

#include <stdlib.h> // NULL, malloc, free
#include <string.h> // strlen

enum dna {
  DNA_G = 'G',
  DNA_C = 'C',
  DNA_T = 'T',
  DNA_A = 'A'
};

enum rna {
  RNA_C = 'C',
  RNA_G = 'G',
  RNA_A = 'A',
  RNA_U = 'U'
};

char *to_rna(const char *dna);

#endif
