module Year
  extend self

  private LEAP_YEAR         =   4
  private CENTURIAL_YEAR    = 100
  private LEAP_CYCLE_LENGTH = LEAP_YEAR * CENTURIAL_YEAR

  def leap?(year : Int) : Bool
    leap_year?(year) && (non_centurial_year?(year) || leap_cycle_year?(year))
  end

  private def leap_year?(year : Int) : Bool
    year.divisible_by?(LEAP_YEAR)
  end

  private def non_centurial_year?(year : Int) : Bool
    !year.divisible_by?(CENTURIAL_YEAR)
  end

  private def leap_cycle_year?(year : Int) : Bool
    year.divisible_by?(LEAP_CYCLE_LENGTH)
  end
end
