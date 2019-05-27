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
        acc.prepend(coin)
        case acc.sum <=> target
        when 1 # greater
          acc.shift
          if coin == denoms.last
            acc.delete(coin)
            if !acc.empty?
              idx = denoms.index(acc.first) + 1
              remaining_denoms = denoms[idx..-1]
              acc.shift
              denominations.append(*remaining_denoms)
            end
          end
        when 0 # equal
          candidates << acc.dup
          idx = denoms.index(acc.last) + 1
          remaining_denoms = denoms[idx..-1]
          acc.clear
          denominations.append(*remaining_denoms)
        else
          redo
        end
        next
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
end
