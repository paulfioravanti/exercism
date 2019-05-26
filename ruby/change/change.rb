require "pry"
module Change
  class ImpossibleCombinationError < StandardError; end
  class NegativeTargetError < StandardError; end

  module_function

  def generate(coins, target)
    return [] if target.zero?
    raise NegativeTargetError if target.negative?
    raise ImpossibleCombinationError if target < coins.min

    denominations = generate_denominations(coins, target)
    a =
      denominations
      .each
      .with_object(denominations.dup)
      .each_with_object([[], []]) do |(coin, denoms), (candidates, acc)|
        next if acc.sum + coin > target

        acc.prepend(coin)
        if acc.sum == target
          candidates << acc.dup
          idx = denoms.index(acc.last) + 1
          acc.clear
          denominations.append(*denoms[idx..-1])
          next
        end
        redo
      end
    a
      .first
      .tap { |candidates| raise ImpossibleCombinationError if candidates.empty? }
      .min_by(&:length)
  end

  def generate_denominations(coins, target)
    coins
      .select { |coin| coin <= target }
      .sort
      .reverse
  end
  private_class_method :generate_denominations

  def initial_tally
    { best: [], candidate: [], index: 0 }
  end
  private_class_method :initial_tally
end
