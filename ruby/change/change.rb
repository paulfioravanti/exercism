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

    # denominations.each_with_object([[]]) do |coin, acc|
    #   (coin..target).each do |subtarget|
    #     best_without = acc.fetch(subtarget - coin, [])

    #     if best_without.nil? ||
    #       acc[subtarget] && acc[subtarget].size <= best_without.size + 1
    #       next
    #     end

    #     acc[subtarget] = [coin] + best_without
    #   end
    #   p coin, target
    #   p acc
    # end
    #   .fetch(target) || (raise ImpossibleCombinationError)

    a =
      denominations
      .each
      .with_object(denominations.dup)
      .each_with_object([[], []]) do |(coin, denoms), (candidates, acc)|
        acc.prepend(coin)
        if acc.sum > target
          acc.shift
          if coin == denoms.last
            idx = denoms.index(acc.first) + 1
            acc.shift
            denominations.append(*denoms[idx..-1])
          end
          next
        elsif acc.sum == target
          candidates << acc.dup
          idx = denoms.index(acc.last) + 1
          acc.clear
          denominations.append(*denoms[idx..-1])
          next
        else
          redo
        end
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
