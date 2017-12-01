defmodule Teenager do
  @only_digits_and_non_words ~r/\A[\d\W]+\z/

  def hey(input) do
    cond do
      silence?(input) ->
        "Fine. Be that way!"
      questioning?(input) ->
        "Sure."
      shouting?(input) ->
        "Whoa, chill out!"
      true ->
        "Whatever."
    end
  end

  defp silence?(input) do
    String.trim(input) == ""
  end

  defp questioning?(input) do
    String.ends_with?(input, "?")
  end

  defp shouting?(input) do
    !String.match?(input, @only_digits_and_non_words) and input == String.upcase(input)
  end
end
