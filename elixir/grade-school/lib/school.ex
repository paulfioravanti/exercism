defmodule School do
  @moduledoc """
  Simulate students in a school.

  Each student is in a grade.
  """

  @doc """
  Add a student to a particular grade in school.
  """
  @spec add(map, String.t(), integer) :: map
  def add(db, name, grade), do: Map.update(db, grade, [name], &[name | &1])

  @doc """
  Return the names of the students in a particular grade.
  """
  @spec grade(map, integer) :: [String.t()]
  def grade(db, grade), do: Map.get(db, grade, [])

  @doc """
  Sorts the school by grade and name.
  """
  @spec sort(map) :: [{integer, [String.t()]}]
  def sort(db) do
    db
    |> Enum.sort()
    |> Enum.map(&to_enrollment/1)
  end

  defp to_enrollment({grade, students}), do: {grade, Enum.sort(students)}
end
