defmodule Zipper do
  defstruct focus: nil, trail: []

  @doc """
  Get a zipper focused on the root node.
  """
  @spec from_tree(BinTree.t()) :: Zipper.t()
  def from_tree(bin_tree), do: %Zipper{focus: bin_tree}

  @doc """
  Get the complete tree from a zipper.
  """
  @spec to_tree(Zipper.t()) :: BinTree.t()
  def to_tree(%Zipper{focus: bin_tree, trail: []}), do: bin_tree

  def to_tree(zipper) do
    zipper
    |> up()
    |> to_tree()
  end

  @doc """
  Get the value of the focus node.
  """
  @spec value(Zipper.t()) :: any
  def value(%Zipper{focus: %BinTree{value: value}}), do: value

  @doc """
  Get the left child of the focus node, if any.
  """
  @spec left(Zipper.t()) :: Zipper.t() | nil
  def left(%Zipper{focus: %BinTree{left: nil}}), do: nil

  def left(%Zipper{focus: focus, trail: trail}) do
    %Zipper{focus: focus.left, trail: [{:left, focus} | trail]}
  end

  @doc """
  Get the right child of the focus node, if any.
  """
  @spec right(Zipper.t()) :: Zipper.t() | nil
  def right(%Zipper{focus: %BinTree{right: nil}}), do: nil

  def right(%Zipper{focus: focus, trail: trail}) do
    %Zipper{focus: focus.right, trail: [{:right, focus} | trail]}
  end

  @doc """
  Get the parent of the focus node, if any.
  """
  @spec up(Zipper.t()) :: Zipper.t() | nil
  def up(%Zipper{trail: []}), do: nil

  def up(%Zipper{focus: focus, trail: [parent | trail]}) do
    %Zipper{focus: parent_tree(parent, focus), trail: trail}
  end

  @doc """
  Set the value of the focus node.
  """
  @spec set_value(Zipper.t(), any) :: Zipper.t()
  def set_value(%Zipper{focus: focus} = zipper, value) do
    %Zipper{zipper | focus: %BinTree{focus | value: value}}
  end

  @doc """
  Replace the left child tree of the focus node.
  """
  @spec set_left(Zipper.t(), BinTree.t() | nil) :: Zipper.t()
  def set_left(%Zipper{focus: focus} = zipper, left) do
    %Zipper{zipper | focus: %BinTree{focus | left: left}}
  end

  @doc """
  Replace the right child tree of the focus node.
  """
  @spec set_right(Zipper.t(), BinTree.t() | nil) :: Zipper.t()
  def set_right(%Zipper{focus: focus} = zipper, right) do
    %Zipper{zipper | focus: %BinTree{focus | right: right}}
  end

  defp parent_tree({:left, parent}, focus), do: %BinTree{parent | left: focus}
  defp parent_tree({:right, parent}, focus), do: %BinTree{parent | right: focus}
end
