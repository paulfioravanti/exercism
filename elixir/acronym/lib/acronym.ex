defmodule Acronym do
  @letter_after_acronym_target ~r/(?!\b\w|[A-Z])./

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
