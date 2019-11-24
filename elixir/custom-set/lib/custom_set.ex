defmodule CustomSet do
  @opaque t :: %__MODULE__{map: map}

  @placeholder nil

  defstruct map: %{}

  @spec new(Enum.t()) :: t
  def new(enumerable) do
    map = Enum.into(enumerable, %{}, &{&1, @placeholder})
    %CustomSet{map: map}
  end

  @spec empty?(t) :: boolean
  def empty?(custom_set) do
    custom_set
    |> Map.get(:map)
    |> Enum.empty?()
  end

  @spec contains?(t, any) :: boolean
  def contains?(custom_set, element) do
    custom_set
    |> Map.get(:map)
    |> Map.has_key?(element)
  end

  @spec subset?(t, t) :: boolean
  def subset?(custom_set_1, custom_set_2) do
    custom_set_1
    |> Map.get(:map)
    |> Map.keys()
    |> Enum.all?(&contains?(custom_set_2, &1))
  end

  @spec disjoint?(t, t) :: boolean
  def disjoint?(custom_set_1, custom_set_2) do
    custom_set_1
    |> Map.get(:map)
    |> Map.keys()
    |> Enum.any?(&contains?(custom_set_2, &1))
    |> Kernel.not()
  end

  @spec equal?(t, t) :: boolean
  def equal?(custom_set_1, custom_set_2) do
    custom_set_1.map == custom_set_2.map
  end

  @spec add(t, any) :: t
  def add(custom_set, element) do
    Map.update!(custom_set, :map, &Map.put(&1, element, @placeholder))
  end

  @spec intersection(t, t) :: t
  def intersection(custom_set_1, custom_set_2) do
    Map.update!(custom_set_1, :map, &map_intersection(custom_set_2, &1))
  end

  @spec difference(t, t) :: t
  def difference(custom_set_1, custom_set_2) do
    Map.update!(custom_set_1, :map, &map_difference(custom_set_2, &1))
  end

  @spec union(t, t) :: t
  def union(custom_set_1, custom_set_2) do
    Map.update!(custom_set_1, :map, &Map.merge(&1, custom_set_2.map))
  end

  defp map_intersection(custom_set, map) do
    map
    |> Map.keys()
    |> Enum.filter(&contains?(custom_set, &1))
    |> Enum.into(%{}, &{&1, @placeholder})
  end

  defp map_difference(custom_set, map) do
    map
    |> Map.keys()
    |> Enum.reject(&contains?(custom_set, &1))
    |> Enum.into(%{}, &{&1, @placeholder})
  end
end
