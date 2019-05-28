class Matrix
  attr_reader :rows, :columns

  def initialize(string)
    @rows = generate_rows(string).freeze
    @columns = rows.transpose.freeze
  end

  def saddle_points
    rows
      .each
      .with_index
      .with_object([], &method(:check_row_saddle_points))
  end

  private

  def generate_rows(string)
    string
      .each_line
      .map(&method(:row_to_integers))
  end

  def row_to_integers(row)
    row
      .split
      .map(&:to_i)
  end

  def check_row_saddle_points((row, row_index), acc)
    row
      .each
      .with_index
      .with_object([row, row_index, acc], &method(:check_column_saddle_points))
  end

  def check_column_saddle_points((value, column_index), (row, row_index, acc))
    return unless saddle_point?(value, row, columns[column_index])

    acc << [row_index, column_index]
  end

  def saddle_point?(value, row, column)
    value == row.max && value == column.min
  end
end
