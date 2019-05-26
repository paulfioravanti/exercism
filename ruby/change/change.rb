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

    denominations.each_with_object([[]]) do |coin, acc|
      (coin..target).each do |subtarget|
        best_without = acc[subtarget - coin]

        if best_without.nil? ||
          acc[subtarget] && acc[subtarget].size <= best_without.size + 1
          next
        end

        acc[subtarget] = [coin] + best_without
      end
    end
      .[](target) || (raise ImpossibleCombinationError)

    # a =
    #   denominations
    #   .each
    #   .with_object(denominations.dup)
    #   .each_with_object([[], []]) do |(coin, denoms), (candidates, acc)|
    #     acc.prepend(coin)
    #     # binding.pry if acc == [4, 4, 4, 5, 5, 5]
    #     # binding.pry if acc == [4, 4, 4, 4, 4, 4, 4]
    #     binding.pry if acc == [2, 20]
    #     case acc.sum <=> target
    #     when 1 # greater
    #       acc.shift
    #       if coin == denoms.last
    #         idx = denoms.index(acc.first) + 1
    #         remaining_denoms = denominations[idx..-1]
    #         # binding.pry
    #         # (idx + remaining_denoms.length).times { acc.shift }
    #         # (idx + acc.uniq.length).times { acc.shift }
    #         remaining_denoms.length.times { acc.shift }
    #         if acc.length > 1
    #           denominations.append(*remaining_denoms)
    #         end
    #       end
    #     when 0 # equal
    #       candidates << acc.dup
    #       idx = denoms.index(acc.last) + 1
    #       remaining_denoms = denoms[idx..-1]
    #       acc.clear
    #       denominations.append(*remaining_denoms)
    #     else
    #       redo
    #     end
    #     next
    #   end
    # a
    #   .first
    #   .tap { |candidates| raise ImpossibleCombinationError if candidates.empty? }
    #   .min_by(&:length)
  end

  def generate_denominations(coins, target)
    coins
      .select { |coin| coin <= target }
      .sort
      .reverse
  end
  private_class_method :generate_denominations
end
