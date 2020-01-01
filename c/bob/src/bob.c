#include "bob.h"

char *hey_bob(char *greeting) {
  if (is_silence(greeting)) {
    return "Fine. Be that way!";
  } else {
    return respond_to_verbal_remark(greeting);
  }
}

bool is_silence(char *greeting) {
  for (int i = 0, len = strlen(greeting); i < len; i++) {
    if (!isspace(greeting[i])) {
      return false;
    }
  }
  return true;
}

char *respond_to_verbal_remark(char *greeting) {
  bool question = is_question(greeting);
  bool yelling = is_yelling(greeting);

  if (question && yelling) {
    return "Calm down, I know what I'm doing!";
  } else if (question) {
    return "Sure.";
  } else if (yelling) {
    return "Whoa, chill out!";
  } else {
    return "Whatever.";
  }
}

bool is_question(char *greeting) {
  char last_char = '\0';
  for (int i = strlen(greeting) - 1; i > -1; i--) {
    if (!isblank(greeting[i])) {
      last_char = greeting[i];
      break;
    }
  }
  return last_char == '?';
}

bool is_yelling(char *greeting) {
  char *upcased = strdup(greeting);
  for (int i = 0, len = strlen(upcased); i < len; i++) {
    upcased[i] = toupper(upcased[i]);
  }

  return has_letters(greeting) && strcmp(upcased, greeting) == 0;
}

bool has_letters(char *greeting) {
  for (int i = 0, len = strlen(greeting); i < len; i++) {
    if (isalpha(greeting[i])) {
      return true;
    }
  }
  return false;
}
