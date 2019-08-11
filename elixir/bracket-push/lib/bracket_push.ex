defmodule BracketPush do
  @opening_brackets ~w|( [ {|
  @closing_brackets ~w|) ] }|

  @doc """
  Checks that all the brackets and braces in the string are matched correctly, and nested correctly
  """
  @spec check_brackets(String.t()) :: boolean
  def check_brackets(str) do
    str
    |> String.graphemes()
    |> Enum.reduce([], &check_pair/2)
    |> Enum.empty?()
  catch
    :halt ->
      false
  end

  defp check_pair(char, acc) do
    cond do
      closing_bracket?(char) && no_matching_opening_bracket?(char, acc) ->
        throw(:halt)

      closing_bracket?(char) ->
        tl(acc)

      opening_bracket?(char) ->
        [char | acc]

      true ->
        acc
    end
  end

  defp opening_bracket?(char) do
    Enum.member?(@opening_brackets, char)
  end

  defp closing_bracket?(char) do
    Enum.member?(@closing_brackets, char)
  end

  defp no_matching_opening_bracket?(_char, []), do: true

  defp no_matching_opening_bracket?(char, acc) do
    opening_bracket =
      @closing_brackets
      |> Enum.find_index(&(&1 == char))
      |> (fn index -> Enum.at(@opening_brackets, index) end).()

    hd(acc) != opening_bracket
  end
end
