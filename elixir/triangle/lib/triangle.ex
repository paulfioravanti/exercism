defmodule Triangle do
  @type kind :: :equilateral | :isosceles | :scalene

  @doc """
  Return the kind of triangle of a triangle with 'a', 'b' and 'c' as lengths.
  """
  @spec kind(number, number, number) :: {:ok, kind} | {:error, String.t()}
  def kind(a, b, c) do
    sides = Enum.sort([a, b, c])

    with {:ok, true} <- all_sides_positive(sides),
         {:ok, true} <- triangle_inequality(sides) do
      sides
      |> Enum.uniq()
      |> Enum.count()
      |> determine_triangle_type()
    end
  end

  defp all_sides_positive(sides) do
    if Enum.all?(sides, &positive?/1) do
      {:ok, true}
    else
      {:error, "all side lengths must be positive"}
    end
  end

  defp positive?(number), do: number > 0

  defp triangle_inequality(sides) do
    [head | tail] = Enum.reverse(sides)

    if Enum.sum(tail) > head do
      {:ok, true}
    else
      {:error, "side lengths violate triangle inequality"}
    end
  end

  defp determine_triangle_type(num_unique_sides) do
    case num_unique_sides do
      1 ->
        {:ok, :equilateral}

      2 ->
        {:ok, :isosceles}

      3 ->
        {:ok, :scalene}
    end
  end
end
