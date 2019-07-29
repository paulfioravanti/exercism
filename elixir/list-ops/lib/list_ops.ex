defmodule ListOps do
  # Please don't use any external modules (especially List or Enum) in your
  # implementation. The point of this exercise is to create these basic
  # functions yourself. You may use basic Kernel functions (like `Kernel.+/2`
  # for adding numbers), but please do not use Kernel functions for Lists like
  # `++`, `--`, `hd`, `tl`, `in`, and `length`.

  @spec count(list) :: non_neg_integer
  def count(l), do: count(l, 0)
  def count([], num), do: num
  def count([head | tail], num), do: count(tail, num + 1)

  @spec reverse(list) :: list
  def reverse(l), do: reverse(l, [])
  def reverse([], reversed), do: reversed
  def reverse([head | tail], reversed), do: reverse(tail, [head | reversed])

  @spec map(list, (any -> any)) :: list
  def map(l, f), do: map(l, [], f)
  def map([], mapped, _f), do: reverse(mapped)
  def map([head | tail], mapped, f), do: map(tail, [f.(head) | mapped], f)

  @spec filter(list, (any -> as_boolean(term))) :: list
  def filter(l, f), do: filter(l, [], f)
  def filter([], filtered, _f), do: reverse(filtered)

  def filter([head | tail], filtered, f) do
    if f.(head) do
      filter(tail, [head | filtered], f)
    else
      filter(tail, filtered, f)
    end
  end

  @type acc :: any
  @spec reduce(list, acc, (any, acc -> acc)) :: acc
  def reduce([], acc, _f), do: acc
  def reduce([head | tail], acc, f), do: reduce(tail, f.(head, acc), f)

  @spec append(list, list) :: list
  def append(a, b), do: do_append(reverse(a), b)
  def do_append([], b), do: b
  def do_append([head | tail], b), do: do_append(tail, [head | b])

  @spec concat([[any]]) :: [any]
  def concat(ll) do
    ll
    |> reverse()
    |> reduce([], &append/2)
  end
end
