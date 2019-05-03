module Year
  LEAP_YEAR = 4
  private_constant :LEAP_YEAR
  CENTURIAL_YEAR = 100
  private_constant :CENTURIAL_YEAR
  LEAP_CYCLE_LENGTH = 400
  private_constant :LEAP_CYCLE_LENGTH

  module_function

  def leap?(year)
    leap_year?(year) && (non_centurial_year?(year) || leap_cycle_year?(year))
  end

  def leap_year?(year)
    (year % LEAP_YEAR).zero?
  end
  private_class_method :leap_year?

  def non_centurial_year?(year)
    (year % CENTURIAL_YEAR).nonzero?
  end
  private_class_method :non_centurial_year?

  def leap_cycle_year?(year)
    (year % LEAP_CYCLE_LENGTH).zero?
  end
  private_class_method :leap_cycle_year?
end
