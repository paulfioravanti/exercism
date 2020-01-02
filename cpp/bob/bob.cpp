#include "bob.h"

using namespace std;

namespace bob {
  const string hey(const string input) {
    const string greeting = trim(input);
    if (is_silence(greeting)) {
      return "Fine. Be that way!";
    } else {
      return respond_to_verbal_greeting(greeting);
    }
  }

  const string trim(const string input) {
    string greeting = input;
    greeting.erase(
      remove_if(greeting.begin(), greeting.end(), isspace),
      greeting.end()
    );
    return greeting;
  }

  bool is_silence(const string greeting) {
    return all_of(greeting.begin(), greeting.end(), isspace);
  }

  const string respond_to_verbal_greeting(const string greeting) {
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

  bool is_question(const string greeting) {
    return greeting.back() == '?';
  }

  bool is_yelling(const string greeting) {
    bool has_letters = any_of(greeting.begin(), greeting.end(), isalpha);
    bool yelling = none_of(greeting.begin(), greeting.end(), islower);
    return has_letters && yelling;
  }
}
