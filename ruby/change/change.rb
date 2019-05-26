require "pry"
module Change
  class ImpossibleCombinationError < StandardError; end
  class NegativeTargetError < StandardError; end

  module_function

  def generate(coins, target)
    return [] if target.zero?
    raise NegativeTargetError if target.negative?
    raise ImpossibleCombinationError if target < coins.min

    denominations = generate_denominations(coins)
    a =
      denominations
      .each
      .with_object(denominations)
      .with_object(target)
      .each_with_object(initial_tally) do |((coin, denoms), target), acc|
        coin > target && acc[:index] = denoms.index(coin) + 1
        next(acc) if acc[:candidate].sum + coin > target

        acc[:candidate].prepend(coin)
        if acc[:candidate].sum == target
          if acc[:best].empty? || acc[:candidate].length < acc[:best].length
            acc[:best] = acc[:candidate]
            acc[:candidate] = []
          end
          denoms.append(*denoms[(acc[:index] + 1)..-1])
          next(acc)
        end
        redo
      end
    a
      .fetch(:best)
      .tap { |change| raise ImpossibleCombinationError if change.empty? }
  end

  def generate_denominations(coins)
    coins
      .sort
      .reverse
  end
  private_class_method :generate_denominations

  def initial_tally
    { best: [], candidate: [], index: 0 }
  end
  private_class_method :initial_tally
end
