defmodule Acronym do
  @character_after_acronym_target ~r/
    (?!     # negative lookahead - after asserting that what follows the
            # current position is not:
      \b\w  # 1. a border character then a letter
      |     # or
      [A-Z] # 2. a capital letter
    )       # (ie an acronym target)
    .       # match any single character
  /x

  @doc """
  Generate an acronym from a string.
  "This is a string" => "TIAS"
  """
  @spec abbreviate(String.t()) :: String.t()
  def abbreviate(string) do
    string
    |> String.replace(@character_after_acronym_target, "")
    |> String.upcase()
  end
end
