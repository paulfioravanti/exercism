struct Year {
  private static let LEAP_YEAR: Int = 4
  private static let CENTURIAL_YEAR: Int = 100
  private static let LEAP_CYCLE_LENGTH: Int = LEAP_YEAR * CENTURIAL_YEAR

  private let calendarYear: Int

  init(calendarYear: Int) {
    self.calendarYear = calendarYear
  }

  func isLeapYear() -> Bool {
    isLeap() && (isNonCenturialYear() || isLeapCycleYear())
  }

  private func isLeap() -> Bool {
    calendarYear.isMultiple(of: Year.LEAP_YEAR)
  }

  private func isNonCenturialYear() -> Bool {
    !calendarYear.isMultiple(of: Year.CENTURIAL_YEAR)
  }

  private func isLeapCycleYear() -> Bool {
    calendarYear.isMultiple(of: Year.LEAP_CYCLE_LENGTH)
  }
}
