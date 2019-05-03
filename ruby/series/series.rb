class Series
  def initialize(string)
    @digits = string
  end

  def slices(length)
    raise ArgumentError if length > digits.length

    digits.chars.each_cons(length).map(&:join)
  end

  private

  attr_reader :digits
end
