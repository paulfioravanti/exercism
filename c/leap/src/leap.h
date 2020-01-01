#ifndef LEAP_H
#define LEAP_H

#include <stdbool.h>

const int LEAP_YEAR;
const int CENTURIAL_YEAR;
const int LEAP_CYCLE_LENGTH;

bool is_leap_year(int year);

bool leap_year(int year);

bool is_non_centurial_year(int year);

bool is_leap_year_cycle(int year);


#endif
