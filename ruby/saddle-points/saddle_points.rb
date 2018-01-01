class Matrix
  attr_reader :rows, :columns

  def initialize(string)
    @rows = generate_rows(string).freeze
    @columns = rows.transpose.freeze
  end

  def saddle_points
    rows.each.with_index.with_object([]) do |(row, row_index), acc|
      row.each.with_index do |value, column_index|
        if saddle_point?(value, row, columns[column_index])
          acc << [row_index, column_index]
        end
      end
    end
  end

  private

  def generate_rows(string)
    string.each_line.map { |row| row.split.map(&:to_i) }
  end

  def saddle_point?(value, row, column)
    value == row.max && value == column.min
  end
end
