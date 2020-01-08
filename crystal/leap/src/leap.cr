module Year
  extend self

  private LEAP_YEAR         =   4
  private CENTURIAL_YEAR    = 100
  private LEAP_CYCLE_LENGTH = LEAP_YEAR * CENTURIAL_YEAR

  def leap?(year : Int) : Bool
    leap_year?(year) && (non_centurial_year?(year) || leap_cycle_year?(year))
  end

  private def leap_year?(year : Int) : Bool
    (year % LEAP_YEAR).zero?
  end

  private def non_centurial_year?(year : Int) : Bool
    !(year % CENTURIAL_YEAR).zero?
  end

  private def leap_cycle_year?(year : Int) : Bool
    (year % LEAP_CYCLE_LENGTH).zero?
  end
end
