defmodule Acronym do
  @character_after_acronym_target ~r/
    (?!     # negative lookahead - check current position is not:
      \b\w  # 1. a border character then a letter
      |     # or
      [A-Z] # 2. a capital letter
    )       # (ie an acronym target)
    .       # then match any single character after
  /x

  @doc """
  Generate an acronym from a string.
  "This is a string" => "TIAS"
  """
  @spec abbreviate(String.t()) :: String.t()
  def abbreviate(string) do
    string
    |> String.replace(@letter_after_acronym_target, "")
    |> String.upcase()
  end
end
