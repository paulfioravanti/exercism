# frozen_string_literal: true

module Tournament
  DRAW_POINTS = 1
  private_constant :DRAW_POINTS
  INCREMENT = 1
  private_constant :INCREMENT
  INPUT_SEPARATOR = ";"
  private_constant :INPUT_SEPARATOR
  LOSS = "loss"
  private_constant :LOSS
  NEW_TALLY = Hash.new do |hash, key|
    hash[key] = {
      matches_played: 0,
      wins: 0,
      draws: 0,
      losses: 0,
      points: 0
    }
  end.freeze
  private_constant :NEW_TALLY
  NEWLINE = "\n"
  private_constant :NEWLINE
  REPORT_RESULTS = %i[matches_played wins draws losses points].freeze
  private_constant :REPORT_RESULTS
  WIN = "win"
  private_constant :WIN
  WIN_POINTS = 3
  private_constant :WIN_POINTS

  module Table
    SCORE_HEADER = "| MP |  W |  D |  L |  P"
    private_constant :SCORE_HEADER
    SPACE = " "
    private_constant :SPACE
    TABLE_MAX_LENGTH = 55
    private_constant :TABLE_MAX_LENGTH
    TEAM_HEADER = "Team"
    private_constant :TEAM_HEADER

    module_function

    def header
      TEAM_HEADER + space_gap(TEAM_HEADER) + SCORE_HEADER + NEWLINE
    end

    def output(tally)
      tally
        .reduce("", &method(:append_team_results))
        .prepend(header)
    end

    def append_team_results(acc, (team_name, team_results))
      acc +
        team_name +
        space_gap(team_name) +
        team_scores_to_string(team_results) +
        NEWLINE
    end
    private_class_method :append_team_results

    def space_gap(string)
      SPACE * (TABLE_MAX_LENGTH - SCORE_HEADER.length - string.length)
    end
    private_class_method :space_gap

    def team_scores_to_string(team_results)
      REPORT_RESULTS
        .map { |result| "|  #{team_results[result]}" }
        .join(SPACE)
    end
    private_class_method :team_scores_to_string
  end
  private_constant :Table

  module_function

  def tally(input)
    results = input.strip
    return Table.header if results.empty?

    results
      .then(&method(:tally_results))
      .then(&method(:sort_tally))
      .then(&Table.method(:output))
  end

  def tally_results(results)
    results
      .split(NEWLINE)
      .each_with_object(NEW_TALLY.dup, &method(:tally_result))
  end
  private_class_method :tally_results

  def tally_result(result, results_tally)
    result.split(INPUT_SEPARATOR).tap do |team1, team2, outcome|
      team1_tally = results_tally[team1]
      team2_tally = results_tally[team2]
      team1_tally[:matches_played] += 1
      team2_tally[:matches_played] += 1
      tally_outcome(team1_tally, team2_tally, outcome)
    end
  end
  private_class_method :tally_result

  def tally_outcome(team1_tally, team2_tally, outcome)
    case outcome
    when WIN
      tally_win(team1_tally, team2_tally)
    when LOSS
      tally_loss(team1_tally, team2_tally)
    else
      tally_draw(team1_tally, team2_tally)
    end
  end
  private_class_method :tally_outcome

  def tally_win(team1_tally, team2_tally)
    team1_tally[:points] += WIN_POINTS
    team1_tally[:wins] += INCREMENT
    team2_tally[:losses] += INCREMENT
  end
  private_class_method :tally_win

  def tally_loss(team1_tally, team2_tally)
    team1_tally[:losses] += INCREMENT
    team2_tally[:wins] += INCREMENT
    team2_tally[:points] += WIN_POINTS
  end
  private_class_method :tally_loss

  def tally_draw(team1_tally, team2_tally)
    team1_tally[:draws] += INCREMENT
    team2_tally[:draws] += INCREMENT
    team1_tally[:points] += DRAW_POINTS
    team2_tally[:points] += DRAW_POINTS
  end
  private_class_method :tally_draw

  def sort_tally(tally)
    tally.sort_by { |team_name, results| [-results[:points], team_name] }
  end
  private_class_method :sort_tally
end
