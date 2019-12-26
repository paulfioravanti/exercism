const LEAP_YEAR: u64 = 4;
const CENTURIAL_YEAR: u64 = 100;
const LEAP_CYCLE_LENGTH: u64 = LEAP_YEAR * CENTURIAL_YEAR;

pub fn is_leap_year(year: u64) -> bool {
    is_leap(year) && (is_non_centurial_year(year) || is_leap_cycle_year(year))
}

fn is_leap(year: u64) -> bool {
    year % LEAP_YEAR == 0
}

fn is_non_centurial_year(year: u64) -> bool {
    year % CENTURIAL_YEAR != 0
}

fn is_leap_cycle_year(year: u64) -> bool {
    year % LEAP_CYCLE_LENGTH == 0
}
