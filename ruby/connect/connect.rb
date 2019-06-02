# frozen_string_literal: true

class Board
  module ChildCoordinates
    ADJACENT_COORDINATES = lambda do |(y_index, x_index)|
      [x_index - 1, x_index + 1, y_index - 1, y_index + 1]
    end
    private_constant :ADJACENT_COORDINATES

    module_function

    def generate(board, piece, y_index, x_index)
      adjacent_coordinates(board, piece, y_index, x_index)
        .to_enum
        .with_object([board, piece])
        .each_with_object([], &method(:add_valid_child))
    end

    def adjacent_coordinates(board, piece, y_index, x_index)
      coordinates =
        [y_index, x_index]
        .then(&ADJACENT_COORDINATES)
        .freeze
      center_coordinates(y_index, coordinates).tap do |coord_list|
        add_above_coordinates(board, x_index, piece, coordinates, coord_list)
        add_below_coordinates(board, x_index, piece, coordinates, coord_list)
      end
    end
    private_class_method :adjacent_coordinates

    def add_valid_child((coordinate, (board, piece)), acc)
      if coordinate.all?(&method(:non_negative?)) &&
         expected_piece?(board, coordinate, piece)
        acc << coordinate
      end
    end
    private_class_method :add_valid_child

    def non_negative?(number)
      !number.negative?
    end
    private_class_method :non_negative?

    def expected_piece?(board, coordinate, piece)
      y, x = coordinate
      board[y][x] == piece
    end
    private_class_method :expected_piece?

    def center_coordinates(y_index, coordinates)
      left, right, _above, _below = coordinates
      [[y_index, left], [y_index, right]]
    end
    private_class_method :center_coordinates

    def add_above_coordinates(board, x_index, piece, coordinates, coord_list)
      left, right, above, _below = coordinates
      return unless board[above]

      coord_list.push(
        [above, x_index],
        piece == X ? [above, right] : [above, left]
      )
    end
    private_class_method :add_above_coordinates

    def add_below_coordinates(board, x_index, piece, coordinates, coord_list)
      left, right, _above, below = coordinates
      return unless board[below]

      coord_list.push(
        [below, x_index],
        piece == X ? [below, left] : [below, right]
      )
    end
    private_class_method :add_below_coordinates
  end
  private_constant :ChildCoordinates

  INITIAL_COLUMN_INDEX = 0
  private_constant :INITIAL_COLUMN_INDEX
  O = "O"
  private_constant :O
  X = "X"
  private_constant :X

  def initialize(board)
    @board = board.map(&:split)
  end

  def winner
    if win?(board, X)
      X
    elsif win?(rotated_board, O)
      O
    else
      ""
    end
  end

  private

  attr_reader :board

  def win?(board, piece)
    catch(:halt) do
      board
        .each
        .with_index
        .with_object([board, piece], &method(:check_row))
      false
    end
  end

  def check_row((row, y_index), (board, piece))
    return unless row.first == piece

    catch(:break) do
      [[y_index, INITIAL_COLUMN_INDEX]]
        .to_enum(:then)
        .with_object([board, row, piece], &method(:find_path))
    end
  end

  def find_path(stack, (board, row, piece))
    loop do
      y_index, x_index = coordinate = stack.pop
      throw(:halt, false) if y_index.nil?
      throw(:halt, true) if x_index == row.length - 1

      children = ChildCoordinates.generate(board, piece, y_index, x_index)
      add_search_path(stack, coordinate, children)
      throw(:break) if stack.last == coordinate
    end
  end

  def add_search_path(stack, coordinate, children)
    stack.push(coordinate)
    children.each do |child|
      stack.push(child) unless stack.include?(child)
    end
  end

  def rotated_board
    board.transpose.map(&:reverse)
  end
end
