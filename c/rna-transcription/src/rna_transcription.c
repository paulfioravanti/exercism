#include "rna_transcription.h"

static char dna_to_rna(const char dna) {
  switch(dna) {
    case DNA_G:
      return RNA_C;
    case DNA_C:
      return RNA_G;
    case DNA_T:
      return RNA_A;
    case DNA_A:
      return RNA_U;
    default:
      return '\0';
  }
}

char *to_rna(const char *dna) {
  if (dna == NULL) {
    return NULL;
  }

  char *rna = malloc(sizeof(char) * strlen(dna));
  for (int i = 0; dna[i]; i++) {
    char rna_nucleotide = dna_to_rna(dna[i]);
    if (rna_nucleotide) {
      rna[i] = rna_nucleotide;
    } else {
      free(rna);
      return NULL;
    }
  }
  return rna;
}
