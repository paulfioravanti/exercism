# frozen_string_literal: true

module Board
  ADJACENT_COORDINATES = lambda do |y_index, x_index|
    [x_index - 1, x_index + 1, y_index - 1, y_index + 1]
  end
  private_constant :ADJACENT_COORDINATES
  BLANK = " "
  private_constant :BLANK
  MINE = "*"
  private_constant :MINE
  HORIZONTAL_BORDER_START = "+"
  private_constant :HORIZONTAL_BORDER_START
  VERTICAL_BORDER = "|"
  private_constant :VERTICAL_BORDER

  module_function

  def transform(board)
    raise ArgumentError unless all_lines_same_length?(board)

    board
      .each
      .with_index
      .with_object(board)
      .map(&method(:transform_line))
  end

  def transform_line((line, y_index), board)
    return line if line.start_with?(HORIZONTAL_BORDER_START)
    raise ArgumentError unless line.start_with?(VERTICAL_BORDER)

    line
      .chars
      .each
      .with_index
      .with_object([y_index, board])
      .map(&method(:transform_character))
      .join
  end
  private_class_method :transform_line

  def transform_character((char, x_index), (y_index, board))
    return char if board_element?(char)
    raise ArgumentError unless char == BLANK

    adjacent_coordinates(board, y_index, x_index)
      .each
      .with_object(board)
      .sum(&method(:sum_mine))
      .then { |sum| sum.positive? ? sum : char }
  end

  def all_lines_same_length?(board)
    board.map(&:length).uniq.one?
  end
  private_class_method :all_lines_same_length?

  def board_element?(char)
    [VERTICAL_BORDER, MINE].include?(char)
  end
  private_class_method :board_element?

  def adjacent_coordinates(board, y_index, x_index)
    left, right, above, below = ADJACENT_COORDINATES.call(y_index, x_index)
    [[y_index, left], [y_index, right]].then do |coords|
      board[above] &&
        coords += [[above, left], [above, x_index], [above, right]]
      board[below] &&
        coords + [[below, left], [below, x_index], [below, right]]
    end
  end
  private_class_method :adjacent_coordinates

  def sum_mine(((y, x), board))
    board[y][x] == MINE ? 1 : 0
  end
  private_class_method :sum_mine
end
