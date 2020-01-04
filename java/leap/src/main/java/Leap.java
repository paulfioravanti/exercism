class Leap {
    private static final int LEAP_YEAR = 4;
    private static final int CENTURIAL_YEAR = 100;
    private static final int LEAP_CYCLE_LENGTH = LEAP_YEAR * CENTURIAL_YEAR;

    boolean isLeapYear(int year) {
        return isLeap(year) &&
            (isNonCenturialYear(year) || isLeapYearCycle(year));
    }

    private boolean isLeap(int year) {
        return year % LEAP_YEAR == 0;
    }

    private boolean isNonCenturialYear(int year) {
        return !(year % CENTURIAL_YEAR == 0);
    }

    private boolean isLeapYearCycle(int year) {
        return year % LEAP_CYCLE_LENGTH == 0;
    }
}
