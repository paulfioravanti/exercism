const LEAP_YEAR = 4
const CENTURIAL_YEAR = 100
const LEAP_CYCLE_LENGTH = LEAP_YEAR * CENTURIAL_YEAR

export const isLeap = year => {
  return isLeapYear(year) && (isNonCenturialYear(year) || isLeapYearCycle(year))
}

function isLeapYear(year) {
  return year % LEAP_YEAR === 0
}

function isNonCenturialYear(year) {
  return !(year % CENTURIAL_YEAR === 0)
}

function isLeapYearCycle(year) {
  return year % LEAP_CYCLE_LENGTH === 0
}
