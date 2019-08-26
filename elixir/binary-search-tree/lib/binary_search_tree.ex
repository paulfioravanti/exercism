defmodule BinarySearchTree do
  @type bst_node :: %{data: any, left: bst_node | nil, right: bst_node | nil}

  defguardp left_node?(node_value, data) when data <= node_value

  @doc """
  Create a new Binary Search Tree with root's value as the given 'data'
  """
  @spec new(any) :: bst_node
  def new(data) do
    %{data: data, left: nil, right: nil}
  end

  @doc """
  Creates and inserts a node with its value as 'data' into the tree.
  """
  @spec insert(bst_node, any) :: bst_node
  def insert(%{data: node_value, left: nil} = tree, data)
      when left_node?(node_value, data) do
    %{tree | left: new(data)}
  end

  def insert(%{data: node_value, left: left_node} = tree, data)
      when left_node?(node_value, data) do
    %{tree | left: insert(left_node, data)}
  end

  def insert(%{right: nil} = tree, data) do
    %{tree | right: new(data)}
  end

  def insert(%{right: right_node} = tree, data) do
    %{tree | right: insert(right_node, data)}
  end

  @doc """
  Traverses the Binary Search Tree in order and returns a list of each node's data.
  """
  @spec in_order(bst_node) :: [any]
  def in_order(tree) do
    tree
    |> in_order([])
    |> Enum.reverse()
  end

  defp in_order(nil, acc), do: acc
  defp in_order(%{data: data, left: nil, right: nil}, acc), do: [data | acc]

  defp in_order(%{data: data, left: left, right: right}, acc) do
    ordered_left = [data | in_order(left, acc)]
    in_order(right, ordered_left)
  end
end
