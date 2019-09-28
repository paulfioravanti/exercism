defmodule Tournament do
  defmodule Tally do
    alias __MODULE__, as: Tally

    defstruct draws: 0, losses: 0, wins: 0

    @draw_points 1
    @win_points 3

    def init_wins, do: %Tally{wins: 1}
    def init_losses, do: %Tally{losses: 1}
    def init_draws, do: %Tally{draws: 1}

    def wins(%Tally{wins: wins}), do: wins
    def losses(%Tally{losses: losses}), do: losses
    def draws(%Tally{draws: draws}), do: draws

    def increment_wins(%Tally{wins: wins} = tally) do
      %Tally{tally | wins: wins + 1}
    end

    def increment_losses(%Tally{losses: losses} = tally) do
      %Tally{tally | losses: losses + 1}
    end

    def increment_draws(%Tally{draws: draws} = tally) do
      %Tally{tally | draws: draws + 1}
    end

    def matches_played(%Tally{draws: draws, losses: losses, wins: wins}) do
      wins + draws + losses
    end

    def points(%Tally{draws: draws, wins: wins}) do
      wins * @win_points + draws * @draw_points
    end
  end

  defmodule Table do
    @report_results ~w[matches_played wins draws losses points]a
    @team_header "Team"
    @team_column_padding 31
    @score_column_padding 3
    @score_header "| MP |  W |  D |  L |  P"
    @divider "|"

    def output(result) do
      body =
        result
        |> Enum.map(&append_team_results/1)
        |> Enum.join("\n")

      header() <> body
    end

    defp header do
      String.pad_trailing(@team_header, @team_column_padding) <>
        @score_header <>
        "\n"
    end

    defp append_team_results({team_name, tally}) do
      String.pad_trailing(team_name, @team_column_padding) <>
        team_scores_to_string(tally)
    end

    defp team_scores_to_string(tally) do
      @report_results
      |> Enum.map(&score_to_string(tally, &1))
      |> Enum.join(" ")
    end

    defp score_to_string(tally, score) do
      score =
        Tally
        |> apply(score, [tally])
        |> to_string()
        |> String.pad_leading(@score_column_padding)

      @divider <> score
    end
  end

  @input_separator ";"

  @doc """
  Given `input` lines representing two teams and whether the first of them won,
  lost, or reached a draw, separated by semicolons, calculate the statistics
  for each team's number of games played, won, drawn, lost, and total points
  for the season, and return a nicely-formatted string table.

  A win earns a team 3 points, a draw earns 1 point, and a loss earns nothing.

  Order the outcome by most total points for the season, and settle ties by
  listing the teams in alphabetical order.
  """
  @spec tally(input :: list(String.t())) :: String.t()
  def tally(input) do
    input
    |> Enum.reduce(%{}, &tally_result/2)
    |> to_sorted_list()
    |> Table.output()
  end

  defp tally_result(result, acc) do
    result
    |> String.split(@input_separator)
    |> tally_outcome(acc)
  end

  defp tally_outcome([team1, team2, "win"], acc) do
    acc
    |> record_win(team1)
    |> record_loss(team2)
  end

  defp tally_outcome([team1, team2, "loss"], acc) do
    acc
    |> record_loss(team1)
    |> record_win(team2)
  end

  defp tally_outcome([team1, team2, "draw"], acc) do
    acc
    |> record_draw(team1)
    |> record_draw(team2)
  end

  defp tally_outcome(_result, acc), do: acc

  defp record_win(acc, team) do
    Map.update(acc, team, Tally.init_wins(), &Tally.increment_wins/1)
  end

  defp record_loss(acc, team) do
    Map.update(acc, team, Tally.init_losses(), &Tally.increment_losses/1)
  end

  defp record_draw(acc, team) do
    Map.update(acc, team, Tally.init_draws(), &Tally.increment_draws/1)
  end

  defp to_sorted_list(results) do
    results
    |> Map.to_list()
    |> Enum.sort_by(&order_points_descending_then_team_name/1)
  end

  defp order_points_descending_then_team_name({team_name, tally}) do
    {-Tally.points(tally), team_name}
  end
end
