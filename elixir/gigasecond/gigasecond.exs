defmodule Gigasecond do
  @seconds :math.pow(10, 9)
           |> round()

  @doc """
  Calculate a date one billion seconds after an input date.
  """
  @spec from(
          {{pos_integer, pos_integer, pos_integer},
           {pos_integer, pos_integer, pos_integer}}
        ) ::
          :calendar.datetime()

  def from(erl_datetime) do
    erl_datetime
    |> NaiveDateTime.from_erl!()
    |> NaiveDateTime.add(@seconds)
    |> NaiveDateTime.to_erl()
  end
end
