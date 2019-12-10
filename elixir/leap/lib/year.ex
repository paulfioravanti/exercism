defmodule Year do
  @leap_year 4
  @centurial_year 100
  @leap_cycle_length @leap_year * @centurial_year

  defguardp leap?(year)
            when rem(year, @leap_year) == 0 and
                   (rem(year, @centurial_year) != 0 or
                      rem(year, @leap_cycle_length) == 0)

  @doc """
  Returns whether 'year' is a leap year.

  A leap year occurs:

  on every year that is evenly divisible by 4
    except every year that is evenly divisible by 100
      unless the year is also evenly divisible by 400
  """
  @spec leap_year?(non_neg_integer) :: boolean
  def leap_year?(year) when leap?(year), do: true
  def leap_year?(_year), do: false
end
