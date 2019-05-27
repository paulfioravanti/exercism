require "pry"
module Change
  class ImpossibleCombinationError < StandardError; end
  class NegativeTargetError < StandardError; end

  GREATER = 1
  private_constant :GREATER
  EQUAL = 0
  private_constant :EQUAL

  module_function

  def generate(coins, target)
    return [] if target.zero?
    raise NegativeTargetError if target.negative?
    raise ImpossibleCombinationError if target < coins.min

    denominations = generate_denominations(coins, target)

    denominations
      .each
      .with_object(denominations.dup)
      .each_with_object([[], []]) do |(coin, denoms), (candidates, acc)|
        acc.prepend(coin)
        case acc.sum <=> target
        when GREATER
          acc.shift
          if coin == denoms.last
            acc.delete(coin)
            if !acc.empty?
              idx = denoms.index(acc.first) + 1
              remaining_denoms = denoms[idx..-1]
              denominations.append(*remaining_denoms)
              acc.shift
            end
          end
        when EQUAL
          idx = denoms.index(acc.last) + 1
          remaining_denoms = denoms[idx..-1]
          denominations.append(*remaining_denoms)
          candidates << acc.dup
          acc.clear
        else # SMALLER
          redo
        end
        next
      end
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
