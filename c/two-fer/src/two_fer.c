#include "two_fer.h"

void two_fer(char *buffer, const char *name) {
  sprintf(buffer, "One for %s, one for me.", name ? name : "you");
}
