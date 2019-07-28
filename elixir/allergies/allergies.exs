defmodule Allergies do
  use Bitwise, only_operators: true

  @allergens ~w[
    eggs
    peanuts
    shellfish
    strawberries
    tomatoes
    chocolate
    pollen
    cats
  ]

  @doc """
  List the allergies for which the corresponding flag bit is true.
  """
  @spec list(non_neg_integer) :: [String.t()]
  def list(flags) do
    @allergens
    |> Enum.with_index()
    |> Enum.filter(&allergen?(&1, flags))
    |> Enum.map(&elem(&1, 0))
  end

  @doc """
  Returns whether the corresponding flag bit in 'flags' is set for the item.
  """
  @spec allergic_to?(non_neg_integer, String.t()) :: boolean
  def allergic_to?(flags, item) do
    flags
    |> list()
    |> Enum.member?(item)
  end

  defp allergen?({_item, index}, flags) do
    (flags &&& allergen_score(index)) > 0
  end

  defp allergen_score(index) do
    1 <<< index
  end
end
