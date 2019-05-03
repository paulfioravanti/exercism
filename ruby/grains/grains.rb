module Grains
  CHESSBOARD = (1..64).freeze
  private_constant :CHESSBOARD
  BASE = 2
  private_constant :BASE
  OFFSET = 1
  private_constant :OFFSET

  module_function

  def square(number)
    raise ArgumentError unless CHESSBOARD.include?(number)

    BASE**(number - OFFSET)
  end

  def total
    CHESSBOARD.reduce(0) { |grains, number| grains + square(number) }
  end
end
