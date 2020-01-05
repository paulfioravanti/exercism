__LEAP_YEAR = 4
__CENTURIAL_YEAR = 100
__LEAP_CYCLE_LENGTH = __LEAP_YEAR * __CENTURIAL_YEAR


def leap_year(year):
    return (
        __is_leap_year(year) and
        (__is_non_centurial_year(year) or __is_leap_cycle_year(year))
    )


def __is_leap_year(year):
    return year % __LEAP_YEAR == 0


def __is_non_centurial_year(year):
    return not year % __CENTURIAL_YEAR == 0


def __is_leap_cycle_year(year):
    return year % __LEAP_CYCLE_LENGTH == 0
