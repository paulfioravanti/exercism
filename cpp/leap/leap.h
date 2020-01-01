#if !defined(LEAP_H)
#define LEAP_H

namespace leap {
  const int LEAP_YEAR = 4;
  const int CENTURIAL_YEAR = 100;
  const int LEAP_CYCLE_LENGTH = LEAP_YEAR * CENTURIAL_YEAR;

  bool is_leap_year(int year);

  bool leap_year(int year);

  bool is_non_centurial_year(int year);

  bool is_leap_year_cycle(int year);
}

#endif
