#ifndef BOB_H
#define BOB_H

#include <ctype.h> // isalpha, isblank, isspace
#include <stdbool.h> // bool
#include <string.h> // strlen, strdup, strcmp

char *hey_bob(char *greeting);

bool is_silence(char *greeting);

char *respond_to_verbal_remark(char *greeting);

bool is_question(char *greeting);

bool is_yelling(char *greeting);

bool has_letters(char *greeting);

#endif
