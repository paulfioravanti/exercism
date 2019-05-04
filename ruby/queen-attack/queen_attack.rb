class Queens
  BOARD_BOUNDARIES = (0..7).freeze
  private_constant :BOARD_BOUNDARIES

  def initialize(options)
    @positions = options.values
    raise ArgumentError unless valid_positions?
  end

  def attack?
    same_row_or_column? || same_diagonal?
  end

  private

  attr_reader :positions

  def valid_positions?
    positions.flatten.all? { |value| BOARD_BOUNDARIES.cover?(value) }
  end

  def same_row_or_column?
    positions.transpose.any? { |pos1, pos2| pos1 == pos2 }
  end

  def same_diagonal?
    positions.transpose.map { |pos1, pos2| (pos1 - pos2).abs }.uniq.one?
  end
end
