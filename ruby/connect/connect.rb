# frozen_string_literal: true

require "pry"
class Board
  EMPTY = "."
  private_constant :EMPTY

  def initialize(board)
    @board = board.map(&:split)
  end

  def winner
    return "" if empty_board?

    if x_wins?
      "X"
    elsif o_wins?
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

  def x_wins?
    straight_connection_win?(board, "X")
  end

  def o_wins?
    straight_connection_win?(rotated_board, "O")
  end

  def straight_connection_win?(board, piece)
    board
      .each
      .with_object(piece)
      .find(&method(:straight_connection?))
  end

  def rotated_board
    board.transpose.map(&:reverse)
  end

  def straight_connection?((row, piece))
    row.uniq == [piece]
  end
end
