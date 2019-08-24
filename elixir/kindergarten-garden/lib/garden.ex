defmodule Garden do
  @plants %{
    ?C => :clover,
    ?G => :grass,
    ?R => :radishes,
    ?V => :violets
  }
  @default_student_names ~w[
    alice
    bob
    charlie
    david
    eve
    fred
    ginny
    harriet
    ileana
    joseph
    kincaid
    larry
  ]a

  @doc """
    Accepts a string representing the arrangement of cups on a windowsill and a
    list with names of students in the class. The student names list does not
    have to be in alphabetical order.

    It decodes that string into the various gardens for each student and returns
    that information in a map.
  """

  @spec info(String.t(), list) :: map
  def info(info_string, student_names \\ @default_student_names) do
    plants = generate_plants(info_string)

    student_names
    |> Enum.sort()
    |> padded_zip(plants)
    |> Enum.into(%{})
  end

  defp generate_plants(info_string) do
    info_string
    |> String.split("\n")
    |> Enum.map(&to_plant_cup_sets/1)
    |> Enum.zip()
    |> Enum.map(&cup_sets_to_tuple/1)
  end

  defp to_plant_cup_sets(plants_string) do
    plants_string
    |> String.to_charlist()
    |> Enum.map(&@plants[&1])
    |> Enum.chunk_every(2)
  end

  defp cup_sets_to_tuple({cup_set1, cup_set2}) do
    (cup_set1 ++ cup_set2)
    |> List.to_tuple()
  end

  defp padded_zip([student | students_tail], [plant_cup | plant_cups_tail]) do
    [{student, plant_cup} | padded_zip(students_tail, plant_cups_tail)]
  end

  defp padded_zip([student | students_tail], []) do
    [{student, {}} | padded_zip(students_tail, [])]
  end

  defp padded_zip([], _plant_cups), do: []
end
