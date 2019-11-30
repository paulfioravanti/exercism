defmodule Alphametics do
  @type puzzle :: binary
  @type solution :: %{required(?A..?Z) => 0..9}
  @non_uppercase_characters ~r/[^A-Z]/
  @single_digits Enum.to_list(0..9)

  @doc """
  Takes an alphametics puzzle and returns a solution where every letter
  replaced by its number will make a valid equation. Returns `nil` when
  there is no valid solution to the given puzzle.

  ## Examples

      iex> Alphametics.solve("I + BB == ILL")
      %{?I => 1, ?B => 9, ?L => 0}

      iex> Alphametics.solve("A == B")
      nil
  """
  @spec solve(puzzle) :: solution | nil
  def solve(puzzle) do
    letters = to_letters(puzzle)

    @single_digits
    |> permutations(length(letters))
    |> Enum.each(&evaluate_permutation(puzzle, letters, &1))
  end

  defp to_letters(puzzle) do
    puzzle
    |> String.replace(@non_uppercase_characters, "")
    |> String.graphemes()
    |> Enum.uniq()
  end

  defp permutations([], _length), do: [[]]
  defp permutations(_digits, 0), do: [[]]

  defp permutations(digits, length) do
    for digit <- digits,
        rest = digits -- [digit],
        rest <- permutations(rest, length - 1),
        do: [digit | rest]
  end

  defp evaluate_permutation(puzzle, letters, permutation) do
    # IO.inspect(permutation)
    IO.inspect(Enum.join(permutation))
    # IO.inspect(letters)
    IO.inspect(Enum.join(letters))

    expression =
      puzzle
      |> String.replace(Enum.join(letters), Enum.join(permutation))
      |> IO.inspect()
      |> String.graphemes()

    cond do
      leading_zero?(expression) ->
        nil

      evaluate(expression) ->
        solution =
          letters
          |> Enum.zip(permutation)
          |> Enum.into(%{})

        throw({:halt, solution})

      true ->
        nil
    end
  end

  defp leading_zero?(expression) do
    Enum.any?(expression, &String.starts_with?(&1, "0"))
  end

  defp evaluate(expression) do
    expression
    |> Enum.chunk_every(2)
    |> Enum.reduce(0, &accumulate_expression/2)
  end

  defp accumulate_expression([fun, value], acc) do
    # IO.inspect(fun)
    # IO.inspect(value)
    # IO.inspect(acc)
    apply(fun, [acc, value])
  end
end
