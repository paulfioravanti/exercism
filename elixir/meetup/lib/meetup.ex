defmodule Meetup do
  @moduledoc """
  Calculate meetup dates.
  """

  @weekdays %{
    monday: 1,
    tuesday: 2,
    wednesday: 3,
    thursday: 4,
    friday: 5,
    saturday: 6,
    sunday: 7
  }
  @teenth_range 13..19

  @type weekday ::
          :monday
          | :tuesday
          | :wednesday
          | :thursday
          | :friday
          | :saturday
          | :sunday

  @type schedule :: :first | :second | :third | :fourth | :last | :teenth

  @doc """
  Calculate a meetup date.

  The schedule is in which week (1..4, last or "teenth") the meetup date should
  fall.
  """
  @spec meetup(pos_integer, pos_integer, weekday, schedule) :: :calendar.date()
  def meetup(year, month, weekday, schedule) do
    generate_date_range(year, month)
    |> Enum.filter(&date_falls_on_day?(&1, @weekdays[weekday]))
    |> select_date_by_schedule(schedule)
    |> Date.to_erl()
  end

  defp generate_date_range(year, month) do
    {:ok, start_date} = Date.new(year, month, 1)
    {:ok, end_date} = Date.new(year, month, Date.days_in_month(start_date))

    Date.range(start_date, end_date)
  end

  defp date_falls_on_day?(date, day_number) do
    Date.day_of_week(date) == day_number
  end

  defp select_date_by_schedule(dates, :first), do: Enum.at(dates, 0)
  defp select_date_by_schedule(dates, :second), do: Enum.at(dates, 1)
  defp select_date_by_schedule(dates, :third), do: Enum.at(dates, 2)
  defp select_date_by_schedule(dates, :fourth), do: Enum.at(dates, 3)
  defp select_date_by_schedule(dates, :last), do: List.last(dates)

  defp select_date_by_schedule(dates, :teenth) do
    Enum.find(dates, &teenth_date?/1)
  end

  defp teenth_date?(%Date{day: day}), do: day in @teenth_range
end
