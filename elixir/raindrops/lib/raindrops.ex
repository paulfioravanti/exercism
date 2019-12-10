defmodule Raindrops do
  @conversions %{
    3 => "Pling",
    5 => "Plang",
    7 => "Plong"
  }

  @doc """
  Returns a string based on raindrop factors.

  - If the number contains 3 as a prime factor, output 'Pling'.
  - If the number contains 5 as a prime factor, output 'Plang'.
  - If the number contains 7 as a prime factor, output 'Plong'.
  - If the number does not contain 3, 5, or 7 as a prime factor,
    just pass the number's digits straight through.
  """
  @spec convert(pos_integer) :: String.t()
  def convert(number) do
    raindrops = Enum.reduce(@conversions, "", &add_raindrop(number, &1, &2))

    case raindrops do
      "" ->
        to_string(number)

      _ ->
        raindrops
    end
  end

  defp add_raindrop(number, {factor, raindrop}, acc) do
    case rem(number, factor) do
      0 ->
        acc <> raindrop

      _ ->
        acc
    end
  end
end
