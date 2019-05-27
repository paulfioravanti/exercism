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
    change_candidates = generate_change_candidates(denominations, target)
    raise ImpossibleCombinationError if change_candidates.empty?

    change_candidates.min_by(&:length)
  end

  def generate_denominations(coins, target)
    coins
      .select { |coin| coin <= target }
      .sort
      .reverse
  end
  private_class_method :generate_denominations

  def generate_change_candidates(denominations, target)
    coin_types = denominations.dup.freeze

    denominations
      .each
      .with_object(denominations)
      .with_object(coin_types)
      .with_object(target)
      .then(&method(:generate_change))
      .first
  end

  def generate_change(denomination_enumerator)
    denomination_enumerator
      .each_with_object([[], []]) \
      do |(((coin, denominations), coin_types), target), (candidates, acc)|
        acc.prepend(coin)
        case acc.sum <=> target
        when GREATER
          acc.shift
          if coin == coin_types.last
            acc.delete(coin)
            unless acc.empty?
              next_smallest_coin_type_index = coin_types.index(acc.first) + 1
              remaining_coin_types =
                coin_types[next_smallest_coin_type_index..-1]
              denominations.append(*remaining_coin_types)
              acc.shift
            end
          end
        when EQUAL
          next_smallest_coin_type_index = coin_types.index(acc.last) + 1
          remaining_coin_types =
            coin_types[next_smallest_coin_type_index..-1]
          denominations.append(*remaining_coin_types)
          candidates << acc.dup
          acc.clear
        else # SMALLER
          redo
        end
        next
      end
  end
end
