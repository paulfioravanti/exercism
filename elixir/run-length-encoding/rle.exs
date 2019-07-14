defmodule RunLengthEncoder do
  # NOTE: \1 backreferences the single character match in parentheses.
  # The parentheses only serve to provide a match for the backreference.
  @consecutive_data_elements ~r{([A-Za-z\s])\1+}
  @run_length_encoding ~r{(\d+)(\D)}

  defguardp empty?(string) when string == ""

  @doc """
  Generates a string where consecutive elements are represented as a data value and count.
  "AABBBCCCC" => "2A3B4C"
  For this example, assume all input are strings, that are all uppercase letters.
  It should also be able to reconstruct the data into its original form.
  "2A3B4C" => "AABBBCCCC"
  """
  @spec encode(String.t()) :: String.t()
  def encode(string) when empty?(string), do: ""

  def encode(string) do
    @consecutive_data_elements
    |> Regex.replace(string, &compress/2)
  end

  @spec decode(String.t()) :: String.t()
  def decode(string) when empty?(string), do: ""

  def decode(string) do
    @run_length_encoding
    |> Regex.replace(string, &reconstruct/3)
  end

  defp compress(whole_match, letter) do
    whole_match
    |> String.length()
    |> to_string()
    |> Kernel.<>(letter)
  end

  defp reconstruct(_whole_match, count, character) do
    character
    |> String.duplicate(String.to_integer(count))
  end
end
