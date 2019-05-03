module Enumerable
  def accumulate
    return to_enum(:accumulate) { size } unless block_given?

    reduce([]) { |acc, elem| acc << yield(elem) }
  end
end
