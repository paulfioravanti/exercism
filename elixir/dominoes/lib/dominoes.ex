defmodule Dominoes do
  @type domino :: {1..6, 1..6}

  require Integer

  @doc """
  chain?/1 takes a list of domino stones and returns boolean indicating if it's
  possible to make a full chain
  """
  @spec chain?(dominoes :: [domino] | []) :: boolean
  def chain?([]), do: true

  def chain?(dominoes) do
    [head | tail] = dominoes = Enum.map(dominoes, &Tuple.to_list/1)
    eulerian_cycle?(dominoes) and connected_graph?(head, tail)
  end

  # REF: https://en.wikipedia.org/wiki/Eulerian_path
  defp eulerian_cycle?(dominoes) do
    dominoes
    |> List.flatten()
    |> Enum.group_by(& &1)
    |> Map.values()
    |> Enum.map(&length/1)
    |> Enum.all?(&Integer.is_even/1)
  end

  defp connected_graph?(_head, []), do: true

  defp connected_graph?(head, tail) do
    # tail represents a set of disjoints
    # https://en.wikipedia.org/wiki/Intersection_(set_theory)#Intersecting_and_disjoint_sets
    {intersections, tail} = Enum.split_with(tail, &intersects?(&1, head))

    cond do
      Enum.empty?(intersections) ->
        false

      true ->
        intersections
        |> List.flatten()
        |> Enum.concat(head)
        |> connected_graph?(tail)
    end
  end

  defp intersects?(edge, head) do
    edge
    |> to_mapset()
    |> MapSet.intersection(to_mapset(head))
    |> MapSet.to_list()
    |> Enum.any?()
  end

  defp to_mapset(domino), do: Enum.into(domino, MapSet.new())
end
