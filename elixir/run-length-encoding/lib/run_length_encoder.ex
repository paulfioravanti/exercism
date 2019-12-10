defmodule RunLengthEncoder do
  # NOTE: \1 backreferences the single character match in parentheses.
  # The parentheses only serve to provide a match for the backreference.
  @consecutive_data_elements ~r{([A-Za-z\s])\1+}
  @run_length_encoding ~r{\d+\D}

  @doc """
  Generates a string where consecutive elements are represented as a data value and count.
  "AABBBCCCC" => "2A3B4C"
  For this example, assume all input are strings, that are all uppercase letters.
  It should also be able to reconstruct the data into its original form.
  "2A3B4C" => "AABBBCCCC"
  """
  @spec encode(String.t()) :: String.t()
  def encode(""), do: ""

  def encode(string) do
    Regex.replace(@consecutive_data_elements, string, &compress/2)
  end

  @spec decode(String.t()) :: String.t()
  def decode(""), do: ""

  def decode(string) do
    Regex.replace(@run_length_encoding, string, &reconstruct/1)
  end

  defp compress(whole_match, letter) do
    "#{String.length(whole_match)}#{letter}"
  end

  defp reconstruct(whole_match) do
    {count, character} = Integer.parse(whole_match)
    String.duplicate(character, count)
  end
end
