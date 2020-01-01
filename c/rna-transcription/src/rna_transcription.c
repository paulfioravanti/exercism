#include "rna_transcription.h"

static char dna_to_rna(const char dna) {
  switch(dna) {
    case 'G':
      return 'C';
    case 'C':
      return 'G';
    case 'T':
      return 'A';
    case 'A':
      return 'U';
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
