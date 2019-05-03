module Hamming
  module_function

  def compute(strand1, strand2)
    raise ArgumentError unless (strand_size = strand1.size) == strand2.size
    return 0 if strand1 == strand2

    (0..strand_size).count { |i| strand1[i] != strand2[i] }
  end
end
