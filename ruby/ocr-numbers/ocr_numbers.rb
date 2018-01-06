# frozen_string_literal: true

module OcrNumbers
  CONVERSIONS = Hash.new("?").tap do |hash|
    hash[" _ | ||_|   "] = "0"
    hash["     |  |   "] = "1"
    hash[" _  _||_    "] = "2"
    hash[" _  _| _|   "] = "3"
    hash["   |_|  |   "] = "4"
    hash[" _ |_  _|   "] = "5"
    hash[" _ |_ |_|   "] = "6"
    hash[" _   |  |   "] = "7"
    hash[" _ |_||_|   "] = "8"
    hash[" _ |_| _|   "] = "9"
  end.freeze
  private_constant :CONVERSIONS
  HEIGHT = 4
  private_constant :HEIGHT
  WIDTH = 3
  private_constant :WIDTH

  module_function

  def convert(pipes)
    pipes = pipes.split("\n")
    raise ArgumentError unless valid_pipes?(pipes)
    pipes
      .map { |row| row.scan(/.{#{WIDTH}}/) }
      .transpose
      .map { |number_array| number_array.each_slice(HEIGHT).to_a }
      .transpose
      .map { |number_array| ocr_to_number(number_array) }
      .join(",")
  end

  def valid_pipes?(pipes)
    (pipes.length % HEIGHT).zero? &&
      pipes.all? { |pipe| (pipe.length % WIDTH).zero? }
  end
  private_class_method :valid_pipes?

  def ocr_to_number(number_array)
    number_array.map { |arr| CONVERSIONS[arr.join] }.join
  end
  private_class_method :ocr_to_number
end

module BookKeeping
  VERSION = 1
end
