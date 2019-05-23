# frozen_string_literal: true

module Board
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
      .with_object(y_index)
      .with_object(board)
      .map(&method(:transform_character))
      .join
  end
  private_class_method :transform_line

  def transform_character(((char, x_index), y_index), board)
    return char if [VERTICAL_BORDER, MINE].include?(char)
    raise ArgumentError unless char == BLANK

    sum = 0
    if board[y_index - 1]
      if board[y_index - 1][x_index - 1] == MINE
        sum += 1
      end
      if board[y_index - 1][x_index] == MINE
        sum += 1
      end
      if board[y_index - 1][x_index + 1] == MINE
        sum += 1
      end
    end
    if board[y_index][x_index - 1] == MINE
      sum += 1
    end
    if board[y_index][x_index + 1] == MINE
      sum += 1
    end
    if board[y_index + 1]
      if board[y_index + 1][x_index - 1] == MINE
        sum += 1
      end
      if board[y_index + 1][x_index] == MINE
        sum += 1
      end
      if board[y_index + 1][x_index + 1] == MINE
        sum += 1
      end
    end
    sum.zero? ? char : sum
  end

  def all_lines_same_length?(board)
    board.map(&:length).uniq.one?
  end
  private_class_method :all_lines_same_length?
end
