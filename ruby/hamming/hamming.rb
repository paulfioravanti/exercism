module Hamming
  module_function

  def compute(strand1, strand2)
    raise ArgumentError unless strand1.size == strand2.size
    return 0 if strand1 == strand2
    strand1.chars.zip(strand2.chars).reduce(0, &method(:calculate_distance))
  end

  def calculate_distance(distance, (nucleotide1, nucleotide2))
    nucleotide1 != nucleotide2 ? distance + 1 : distance
  end
  private_class_method :calculate_distance
end

module BookKeeping
  VERSION = 3
end
