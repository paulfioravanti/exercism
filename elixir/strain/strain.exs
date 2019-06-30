defmodule Strain do
  @doc """
  Given a `list` of items and a function `fun`, return the list of items where
  `fun` returns true.

  Do not use `Enum.filter`.
  """
  @spec keep(list :: list(any), fun :: (any -> boolean)) :: list(any)
  def keep(list, fun) do
    list
    |> filter(fn elem, acc -> if fun.(elem), do: [elem | acc], else: acc end)
  end

  @doc """
  Given a `list` of items and a function `fun`, return the list of items where
  `fun` returns false.

  Do not use `Enum.reject`.
  """
  @spec discard(list :: list(any), fun :: (any -> boolean)) :: list(any)
  def discard(list, fun) do
    list
    |> keep(fn elem -> not fun.(elem) end)
  end

  defp filter(list, fun) do
    list
    |> Enum.reduce([], &fun.(&1, &2))
    |> Enum.reverse()
  end
end
