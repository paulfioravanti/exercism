defmodule Sublist do
  defguardp shorter?(x, y) when length(x) < length(y)

  @doc """
  Returns whether the first list is a sublist or a superlist of the second list
  and if not whether it is equal or unequal to the second list.
  """
  def compare(a, b) do
    cond do
      a === b ->
        :equal

      b |> contains?(a) ->
        :sublist

      a |> contains?(b) ->
        :superlist

      true ->
        :unequal
    end
  end

  defp contains?(x, y) when x |> shorter?(y), do: false

  defp contains?(x, y) do
    x |> List.starts_with?(y) or tl(x) |> contains?(y)
  end
end
