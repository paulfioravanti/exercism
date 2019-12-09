defmodule BinarySearch do
  # NOTE: By the time this guard has been reached, search options will have
  # been exhausted, and the `left` index will have been incremented, or the
  # `right` index decremented, to a number where we would be attempting to
  # perform an out-of-bounds search on position `left` of a "sub-tuple" of
  # length `right`.
  defguardp key_not_found?(left, right) when left > right

  @doc """
    Searches for a key in the tuple using the binary search algorithm.
    It returns :not_found if the key is not in the tuple.
    Otherwise returns {:ok, index}.

    ## Examples

      iex> BinarySearch.search({}, 2)
      :not_found

      iex> BinarySearch.search({1, 3, 5}, 2)
      :not_found

      iex> BinarySearch.search({1, 3, 5}, 5)
      {:ok, 2}

  """
  @spec search(tuple, integer) :: {:ok, integer} | :not_found
  def search({}, _key), do: :not_found
  def search(numbers, key), do: search(numbers, key, 0, tuple_size(numbers) - 1)

  defp search(_numbers, _key, left, right) when key_not_found?(left, right) do
    :not_found
  end

  defp search(numbers, key, left, right) do
    middle_index = div(left + right, 2)
    middle_element = elem(numbers, middle_index)

    cond do
      middle_element > key ->
        search(numbers, key, left, middle_index - 1)

      middle_element < key ->
        search(numbers, key, middle_index + 1, right)

      true ->
        {:ok, middle_index}
    end
  end
end
