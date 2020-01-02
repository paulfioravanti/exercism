#if !defined(BOB_H)
#define BOB_H

#include <string>

namespace bob {
  const std::string hey(const std::string);

  const std::string trim(const std::string);

  bool is_silence(const std::string);

  const std::string respond_to_verbal_greeting(const std::string);

  bool is_question(const std::string);

  bool is_yelling(const std::string);
}

#endif
