defmodule ListOps do
  # Please don't use any external modules (especially List or Enum) in your
  # implementation. The point of this exercise is to create these basic
  # functions yourself. You may use basic Kernel functions (like `Kernel.+/2`
  # for adding numbers), but please do not use Kernel functions for Lists like
  # `++`, `--`, `hd`, `tl`, `in`, and `length`.

  @spec count(list) :: non_neg_integer
  def count(l), do: count(l, 0)
  defp count([], acc), do: acc
  defp count([_head | tail], acc), do: count(tail, acc + 1)

  @spec reverse(list) :: list
  def reverse(l), do: reverse(l, [])
  defp reverse([], acc), do: acc
  defp reverse([head | tail], acc), do: reverse(tail, [head | acc])

  @spec map(list, (any -> any)) :: list
  def map(l, f), do: map(l, [], f)
  defp map([], acc, _f), do: reverse(acc)
  defp map([head | tail], acc, f), do: map(tail, [f.(head) | acc], f)

  @spec filter(list, (any -> as_boolean(term))) :: list
  def filter(l, f), do: filter(l, [], f)
  defp filter([], acc, _f), do: reverse(acc)

  defp filter([head | tail], acc, f) do
    if f.(head) do
      filter(tail, [head | acc], f)
    else
      filter(tail, acc, f)
    end
  end

  @type acc :: any
  @spec reduce(list, acc, (any, acc -> acc)) :: acc
  def reduce([], acc, _f), do: acc
  def reduce([head | tail], acc, f), do: reduce(tail, f.(head, acc), f)

  @spec append(list, list) :: list
  def append(a, b) do
    a
    |> reverse()
    |> reduce(b, &[&1 | &2])
  end

  @spec concat([[any]]) :: [any]
  def concat(ll) do
    ll
    |> reverse()
    |> reduce([], &append/2)
  end
end
