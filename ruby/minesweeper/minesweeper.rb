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
    raise ArgumentError unless board.map(&:length).uniq.one?

    board
      .each
      .with_index
      .with_object(board)
      .each_with_object([]) do |((line, y_index), board), acc|
        next(acc << line) if line.start_with?(HORIZONTAL_BORDER_START)
        raise ArgumentError unless line.start_with?(VERTICAL_BORDER)

        l =
          line
          .chars
          .each
          .with_index
          .with_object(y_index)
          .with_object(board)
          .each_with_object([]) do |(((char, x_index), y_index), board), acc2|
            next(acc2 << char) if [VERTICAL_BORDER, MINE].include?(char)
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

            acc2 << (sum.zero? ? char : sum)
          end
          .join
        acc << l
      end
  end
end
