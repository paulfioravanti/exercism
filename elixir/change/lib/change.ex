defmodule Change do
  @error_message "cannot change"

  @doc """
    Determine the least number of coins to be given to the user such
    that the sum of the coins' value would equal the correct amount of change.
    It returns {:error, "cannot change"} if it is not possible to compute the
    right amount of coins. Otherwise returns the tuple {:ok, list_of_coins}

    ## Examples

      iex> Change.generate([5, 10, 15], 3)
      {:error, "cannot change"}

      iex> Change.generate([1, 5, 10], 18)
      {:ok, [1, 1, 1, 5, 10]}

  """

  @spec generate(list, integer) :: {:ok, list} | {:error, String.t()}
  def generate(_coins, 0), do: {:ok, []}
  def generate(_coins, target) when target < 0, do: {:error, @error_message}

  def generate(coins, target) do
    if target < Enum.min(coins) do
      {:error, @error_message}
    else
      {_coins, change_candidates, _acc} =
        coins
        |> generate_useable_coins(target)
        |> generate_change_candidates(target)

      case change_candidates do
        [] ->
          {:error, @error_message}

        _ ->
          fewest_coin_set = Enum.min_by(change_candidates, &length/1)
          {:ok, fewest_coin_set}
      end
    end
  end

  defp generate_useable_coins(coins, target) do
    coins
    |> Enum.filter(&(&1 <= target))
    |> Enum.sort(&(&1 >= &2))
  end

  defp generate_change_candidates(coins, target) do
    coins
    |> Enum.reduce({coins, [], []}, &generate_change(coins, target, &1, &2))
  end

  defp generate_change(coins, target, coin, {change_coins, candidates, acc}) do
    combination = [coin | acc]
    sum = Enum.sum(combination)

    cond do
      sum > target ->
        case change_coins do
          [] ->
            handle_possible_change_without_unit_coins(coins, candidates, acc)

          [head | []] ->
            generate_change(coins, target, head, {[], candidates, acc})

          [_head | tail] ->
            generate_change(coins, target, hd(tail), {tail, candidates, acc})
        end

      sum == target ->
        change_coins =
          if Enum.empty?(change_coins) do
            []
          else
            tl(change_coins)
          end

        {change_coins, [combination | candidates], []}

      # sum < target
      true ->
        generate_change(
          coins,
          target,
          coin,
          {change_coins, candidates, combination}
        )
    end
  end

  defp handle_possible_change_without_unit_coins(coins, candidates, acc) do
    case length(candidates) do
      0 ->
        {coins, candidates, Enum.drop(acc, 1)}

      _ ->
        {[], candidates, acc}
    end
  end
end
