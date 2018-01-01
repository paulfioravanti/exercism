class Matrix
  attr_reader :rows, :columns

  def initialize(string)
    @rows = generate_rows(string).freeze
    @columns = rows.transpose.freeze
  end

  private

  def generate_rows(string)
    string.each_line.map { |row| row.split.map(&:to_i) }
  end
end
