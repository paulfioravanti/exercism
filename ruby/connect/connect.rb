# frozen_string_literal: true

require "pry"
class Board
  EMPTY = "."
  private_constant :EMPTY
  PIECE_TYPES = %w[O X].freeze
  private_constant :PIECE_TYPES

  def initialize(board)
    @board = board.map(&:split)
  end

  def winner
    return "" if empty_board?

    connection = straight_connection
    connection || ""
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

  def straight_connection
    connection =
      board.find(&method(:connection)) ||
      rotated_board.find(&method(:connection))
    connection&.first
  end

  def rotated_board
    board.transpose.map(&:reverse)
  end

  def connection(row)
    piece_types = row.uniq
    return unless piece_types.one?

    piece_type = piece_types.first
    return unless PIECE_TYPES.include?(piece_type)

    piece_type
  end
end
