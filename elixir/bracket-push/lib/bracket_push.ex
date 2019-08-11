defmodule BracketPush do
  @opening_brackets '([{'
  @closing_brackets ')]}'

  defguardp opening_bracket?(char) when char in @opening_brackets
  defguardp closing_bracket?(char) when char in @closing_brackets

  @doc """
  Checks that all the brackets and braces in the string are matched correctly, and nested correctly
  """
  @spec check_brackets(String.t()) :: boolean
  def check_brackets(str) do
    str
    |> String.to_charlist()
    |> Enum.reduce([], &check_pair/2)
    |> Enum.empty?()
  catch
    :halt ->
      false
  end

  defp check_pair(char, acc) when opening_bracket?(char), do: [char | acc]

  defp check_pair(char, acc) when closing_bracket?(char) do
    if matching_opening_bracket?(char, acc) do
      tl(acc)
    else
      throw(:halt)
    end
  end

  defp check_pair(_char, acc), do: acc

  defp matching_opening_bracket?(_char, []), do: false

  defp matching_opening_bracket?(char, acc) do
    opening_bracket =
      @closing_brackets
      |> Enum.find_index(&(&1 == char))
      |> (fn index -> Enum.at(@opening_brackets, index) end).()

    hd(acc) == opening_bracket
  end
end
