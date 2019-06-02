# frozen_string_literal: true

require "pry"
class Board
  ADJACENT_COORDINATES = lambda do |y_index, x_index|
    [x_index - 1, x_index + 1, y_index - 1, y_index + 1]
  end
  EMPTY = "."
  private_constant :EMPTY

  def initialize(board)
    @board = board.map(&:split)
  end

  def winner
    return "" if empty_board?

    if win?(board, "X")
      "X"
    elsif win?(rotated_board, "O")
      "O"
    else
      ""
    end
  end

  private

  attr_reader :board

  def empty_board?
    board.all?(&method(:empty_rows?))
  end

  def empty_rows?(row)
    row.all?(&method(:empty_column?))
  end

  def empty_column?(column)
    column == EMPTY
  end

  def win?(board, piece)
    board.each.with_index do |row, y_index|
      next unless row.first == piece

      stack = [[y_index, 0]]
      loop do
        y_index, x_index = node = stack.pop
        return false if y_index.nil?
        return true if x_index == row.length - 1

        children =
          adjacent_coordinates(board, piece, y_index, x_index)
          .select do |child|
            y_coord, x_coord = child
            child.all? { |n| !n.negative? } &&
              board[y_coord][x_coord] == piece
          end

        stack.push(node)
        children.each do |child|
          stack.push(child) unless stack.include?(child)
        end
        break if stack.last == node
      end
    end
    false
  end

  def adjacent_coordinates(board, piece, y_index, x_index)
    left, right, above, below = ADJACENT_COORDINATES.call(y_index, x_index)
    coords = [[y_index, left], [y_index, right]]
    if board[above]
      coords +=
        if piece == "X"
          [[above, x_index], [above, right]]
        else
          [[above, x_index], [above, left]]
        end
    end
    if board[below]
      coords +=
        if piece == "X"
          [[below, x_index], [below, left]]
        else
          [[below, x_index], [below, right]]
        end
    end
    coords
  end

  def rotated_board
    board.transpose.map(&:reverse)
  end
end
