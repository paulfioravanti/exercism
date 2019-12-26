defmodule Year do
  @leap_year 4
  @centurial_year 100
  @leap_cycle_length @leap_year * @centurial_year

  defguardp is_leap_year(year) when rem(year, @leap_year) == 0
  defguardp is_not_centurial_year(year) when rem(year, @centurial_year) != 0
  defguardp is_leap_cycle_year(year) when rem(year, @leap_cycle_length) == 0

  defguardp is_leap(year)
            when is_leap_year(year) and
                   (is_not_centurial_year(year) or is_leap_cycle_year(year))

  @doc """
  Returns whether 'year' is a leap year.

  A leap year occurs:

  on every year that is evenly divisible by 4
    except every year that is evenly divisible by 100
      unless the year is also evenly divisible by 400
  """
  @spec leap_year?(non_neg_integer) :: boolean
  def leap_year?(year) when is_leap(year), do: true
  def leap_year?(_year), do: false
end
