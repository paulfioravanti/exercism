defmodule Dot do
  # Empty Graph
  defmacro graph(do: {:__block__, _meta, []}), do: Macro.escape(%Graph{})

  defmacro graph(do: ast) do
    %Graph{}
    |> transform(ast)
    |> Macro.escape()
  end

  defp transform(graph, ast) when is_list(ast) do
    Enum.reduce(ast, graph, &transform(&2, &1))
  end

  defp transform(graph, {:__block__, _meta, args}), do: transform(graph, args)

  defp transform(%Graph{attrs: attrs} = graph, {:graph, _meta, args}) do
    attrs = parse_attrs([args | attrs])
    %Graph{graph | attrs: attrs}
  end

  defp transform(
         %Graph{edges: edges} = graph,
         {:--, _meta, [{node1, _meta1, _args}, {node2, _meta2, args}]}
       ) do
    attrs = parse_attrs(args || [])
    edges = [{node1, node2, attrs} | edges]
    %Graph{graph | edges: edges}
  end

  defp transform(%Graph{nodes: nodes} = graph, {node, _meta, args})
       when is_atom(node) do
    attrs = parse_attrs(args || [])
    nodes = Enum.sort([{node, attrs} | nodes])
    %Graph{graph | nodes: nodes}
  end

  defp transform(_graph, _ast), do: raise(ArgumentError)

  defp parse_attrs(attrs) do
    attrs = List.flatten(attrs)

    if Keyword.keyword?(attrs) do
      Enum.sort(attrs)
    else
      raise ArgumentError
    end
  end
end
