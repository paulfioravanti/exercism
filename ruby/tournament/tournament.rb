# frozen_string_literal: true

module Tournament
  INPUT_SEPARATOR = ";"
  NEW_TALLY = Hash.new do |hash, key|
    hash[key] = {
      matches_played: 0,
      wins: 0,
      draws: 0,
      losses: 0,
      points: 0
    }
  end.freeze
  NEWLINE = "\n"
  REPORT_RESULTS = %i[matches_played wins draws losses points].freeze
  SCORE_HEADER = "| MP |  W |  D |  L |  P"
  TABLE_MAX_LENGTH = 55
  TEAM_HEADER = "Team"

  module_function

  def tally(input)
    results = input.strip
    return header if results.empty?

    results_tally = NEW_TALLY.dup
    process_results(results, results_tally)

    results_tally
      .then(&method(:sort_tally))
      .reduce("", &method(:append_team_results))
      .prepend(header)
  end

  def header
    TEAM_HEADER + space_gap(TEAM_HEADER) + SCORE_HEADER + NEWLINE
  end

  def process_results(results, results_tally)
    results.split(NEWLINE).each do |result|
      result.split(INPUT_SEPARATOR).tap do |team1, team2, outcome|
        process_result(team1, team2, outcome, results_tally)
      end
    end
  end

  def process_result(team1, team2, outcome, results_tally)
    team1_tally = results_tally[team1]
    team2_tally = results_tally[team2]
    team1_tally[:matches_played] += 1
    team2_tally[:matches_played] += 1
    tally_outcome(team1_tally, team2_tally, outcome)
  end

  def tally_outcome(team1_tally, team2_tally, outcome)
    case outcome
    when "win"
      tally_win(team1_tally, team2_tally)
    when "loss"
      tally_loss(team1_tally, team2_tally)
    else
      tally_draw(team1_tally, team2_tally)
    end
  end

  def tally_win(team1_tally, team2_tally)
    team1_tally[:points] += 3
    team1_tally[:wins] += 1
    team2_tally[:losses] += 1
  end

  def tally_loss(team1_tally, team2_tally)
    team1_tally[:losses] += 1
    team2_tally[:wins] += 1
    team2_tally[:points] += 3
  end

  def tally_draw(team1_tally, team2_tally)
    team1_tally[:draws] += 1
    team2_tally[:draws] += 1
    team1_tally[:points] += 1
    team2_tally[:points] += 1
  end

  def append_team_results(acc, (team_name, team_results))
    acc +
      team_name +
      space_gap(team_name) +
      team_scores_to_string(team_results) +
      NEWLINE
  end

  def team_scores_to_string(team_results)
    REPORT_RESULTS
      .map { |result| "|  #{team_results[result]}" }
      .join(" ")
  end

  def space_gap(name)
    " " * (TABLE_MAX_LENGTH - SCORE_HEADER.length - name.length)
  end

  def sort_tally(tally)
    tally.sort_by { |team_name, results| [-results[:points], team_name] }
  end
end
