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
    |> Enum.reduce_while(nil, &evaluate_permutation(puzzle, letters, &1, &2))
  end

  defp evaluate_permutation(puzzle, letters, permutation, acc) do
    translations = Enum.zip(letters, permutation)
    expression = generate_expression(puzzle, translations)

    cond do
      any_leading_zeroes?(expression) ->
        {:cont, acc}

      evaluate(expression) ->
        solution = Enum.reduce(translations, %{}, &add_char_key_value/2)
        {:halt, solution}

      true ->
        {:cont, acc}
    end
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
        rest <- permutations(rest, length - 1) do
      [digit | rest]
    end
  end

  defp generate_expression(puzzle, translations) do
    translations
    |> Enum.reduce(puzzle, &letter_to_permutation/2)
    |> String.split()
  end

  defp letter_to_permutation({letter, permutation}, acc) do
    String.replace(acc, letter, Integer.to_string(permutation))
  end

  defp any_leading_zeroes?(expression) do
    Enum.any?(expression, &String.starts_with?(&1, "0"))
  end

  defp evaluate([head | tail]) do
    tail
    |> Enum.chunk_every(2)
    |> Enum.reduce(String.to_integer(head), &accumulate_expression/2)
  end

  defp accumulate_expression([fun, value], acc) do
    apply(Kernel, String.to_atom(fun), [acc, String.to_integer(value)])
  end

  defp add_char_key_value({<<translation::utf8>>, value}, acc) do
    Map.put(acc, translation, value)
  end
end
