#if !defined(LEAP_H)
#define LEAP_H

namespace leap {
  bool is_leap_year(int year);

  namespace detail {
    static const int LEAP_YEAR = 4;
    static const int CENTURIAL_YEAR = 100;
    static const int LEAP_CYCLE_LENGTH = LEAP_YEAR * CENTURIAL_YEAR;

    bool leap_year(int year);

    bool is_non_centurial_year(int year);

    bool is_leap_year_cycle(int year);
  }
}

#endif
