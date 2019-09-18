defmodule Meetup do
  @moduledoc """
  Calculate meetup dates.
  """

  @day_numbers %{
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
    |> Enum.filter(&date_falls_on_day?(&1, @day_numbers[weekday]))
    |> select_date_by_schedule(schedule)
  end

  defp generate_date_range(year, month) do
    {:ok, start_date} = Date.new(year, month, 1)
    last_day_of_the_month = :calendar.last_day_of_the_month(year, month)
    {:ok, end_date} = Date.new(year, month, last_day_of_the_month)

    Date.range(start_date, end_date)
    |> Enum.map(&Date.to_erl/1)
  end

  defp date_falls_on_day?({year, month, day}, day_number) do
    :calendar.day_of_the_week(year, month, day) == day_number
  end

  defp select_date_by_schedule(dates, :first), do: Enum.at(dates, 0)
  defp select_date_by_schedule(dates, :second), do: Enum.at(dates, 1)
  defp select_date_by_schedule(dates, :third), do: Enum.at(dates, 2)
  defp select_date_by_schedule(dates, :fourth), do: Enum.at(dates, 3)
  defp select_date_by_schedule(dates, :last), do: List.last(dates)

  defp select_date_by_schedule(dates, :teenth) do
    Enum.find(dates, &teenth_date?/1)
  end

  defp teenth_date?({_year, _month, day}), do: day in @teenth_range
end
