defmodule FlattenArray do
  @doc """
    Accept a list and return the list flattened without nil values.

    ## Examples

      iex> FlattenArray.flatten([1, [2], 3, nil])
      [1,2,3]

      iex> FlattenArray.flatten([nil, nil])
      []

  """

  @spec flatten(list) :: list
  def flatten(list), do: flatten(list, [])

  defp flatten([], acc), do: acc
  defp flatten([nil | tail], acc), do: flatten(tail, acc)

  defp flatten([head | tail], acc) when is_list(head) do
    flattened_tail = flatten(tail, acc)
    flatten(head, flattened_tail)
  end

  defp flatten([head | tail], acc), do: [head | flatten(tail, acc)]
end
