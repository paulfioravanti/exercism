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

      contains?(b, a) ->
        :sublist

      contains?(a, b) ->
        :superlist

      true ->
        :unequal
    end
  end

  defp contains?(x, y) when shorter?(x, y), do: false

  defp contains?([_x_head | x_tail] = x, y) do
    List.starts_with?(x, y) or contains?(x_tail, y)
  end
end
