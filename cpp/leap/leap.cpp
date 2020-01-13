#include "leap.h"

namespace leap {
  bool is_leap_year(int year) {
    using namespace detail;

    return leap_year(year) &&
      (is_non_centurial_year(year) || is_leap_year_cycle(year));
  }

  namespace detail {
    bool leap_year(int year) {
      return year % LEAP_YEAR == 0;
    }

    bool is_non_centurial_year(int year) {
      return !(year % CENTURIAL_YEAR == 0);
    }

    bool is_leap_year_cycle(int year) {
      return year % LEAP_CYCLE_LENGTH == 0;
    }
  }
}
