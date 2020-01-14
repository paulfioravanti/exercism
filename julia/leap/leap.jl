const LEAP_YEAR = 4
const CENTURIAL_YEAR = 100
const LEAP_CYCLE_LENGTH = LEAP_YEAR * CENTURIAL_YEAR

function is_leap_year(year::Int)
    is_leap(year) && (is_non_centurial(year) || is_leap_cycle(year))
end

function is_leap(year::Int)
    year % LEAP_YEAR == 0
end

function is_non_centurial(year::Int)
    year % CENTURIAL_YEAR != 0
end

function is_leap_cycle(year::Int)
    year % LEAP_CYCLE_LENGTH == 0
end
