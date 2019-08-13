defmodule BinarySearch do
  defguardp key_too_high?(left, right) when left > right

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

  def search(numbers, key) do
    search(numbers, key, 0, tuple_size(numbers) - 1)
  end

  defp search(_numbers, _key, left, right) when key_too_high?(left, right) do
    :not_found
  end

  defp search(numbers, key, left, right) do
    middle_index = div(left + right, 2)
    middle_element = elem(numbers, middle_index)

    cond do
      key > middle_element ->
        search(numbers, key, middle_index + 1, right)

      key < middle_element ->
        search(numbers, key, left, middle_index - 1)

      true ->
        {:ok, middle_index}
    end
  end
end
