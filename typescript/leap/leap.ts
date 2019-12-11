const LEAP_YEAR = 4
const CENTURIAL_YEAR = 100

function isLeapYear(year: number): boolean {
  return isLeap(year) && (isNonCenturialYear(year) || isLeapYearCycle(year))
}

function isLeap(year: number): boolean {
  return year % LEAP_YEAR === 0
}

function isNonCenturialYear(year: number): boolean {
  return !(year % CENTURIAL_YEAR === 0)
}

function isLeapYearCycle(year: number): boolean {
  const leapCycleLength: number = LEAP_YEAR * CENTURIAL_YEAR
  return year % leapCycleLength === 0
}

export default isLeapYear
