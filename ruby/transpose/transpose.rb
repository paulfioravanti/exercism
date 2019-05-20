# frozen_string_literal: true

module Transpose
  NEWLINE = "\n"
  private_constant :NEWLINE
  SPACE = " "
  private_constant :SPACE

  module_function

  def transpose(input)
    return "" if input.empty?

    lines = generate_lines(input)
    max_line_width = max_line_width(lines)
    lines
      .each
      .with_object(max_line_width)
      .map(&method(:right_pad_short_line_with_nils))
      .transpose
      .then(&method(:revert_nil_padding))
      .join(NEWLINE)
  end

  def generate_lines(input)
    input
      .split(NEWLINE)
      .map(&:chars)
  end
  private_class_method :generate_lines

  def max_line_width(lines)
    lines
      .map(&:length)
      .max
  end
  private_class_method :max_line_width

  def right_pad_short_line_with_nils(line, max_line_width)
    line.values_at(0...max_line_width)
  end
  private_class_method :right_pad_short_line_with_nils

  def revert_nil_padding(lines)
    lines
      .lazy
      .map(&method(:remove_right_padded_nils))
      .map(&method(:replace_nils_with_spaces))
      .map(&:join)
      .to_a
  end
  private_class_method :revert_nil_padding

  def remove_right_padded_nils(line)
    line
      .reverse
      .drop_while(&:nil?)
      .reverse
  end
  private_class_method :remove_right_padded_nils

  def replace_nils_with_spaces(line)
    line.map { |char| char || SPACE }
  end
  private_class_method :replace_nils_with_spaces
end
