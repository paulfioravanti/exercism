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
  private_class_method :generate_change_candidates

  def generate_change(denomination_enumerator)
    denomination_enumerator
      .each_with_object([[], []]) \
      do |(((coin, denominations), coin_types), target), (candidates, acc)|
        acc.prepend(coin)
        case acc.sum <=> target
        when GREATER
          try_next_smallest_coin(coin, denominations, coin_types, acc)
        when EQUAL
          store_candidate(denominations, coin_types, candidates, acc)
        else # SMALLER
          redo
        end
        next
      end
  end
  private_class_method :generate_change

  def try_next_smallest_coin(coin, denominations, coin_types, acc)
    acc.shift
    return unless smallest_coin?(coin, coin_types)

    acc.delete(coin)
    return if acc.empty?

    append_coin_type_subset(denominations, coin_types, acc.first)
    acc.shift
  end
  private_class_method :try_next_smallest_coin

  def smallest_coin?(coin, coin_types)
    coin == coin_types.last
  end
  private_class_method :smallest_coin?

  def store_candidate(denominations, coin_types, candidates, acc)
    append_coin_type_subset(denominations, coin_types, acc.last)
    candidates << acc.dup
    acc.clear
  end
  private_class_method :store_candidate

  def append_coin_type_subset(denominations, coin_types, coin)
    next_smallest_coin_type_index = coin_types.index(coin) + 1
    remaining_coin_types = coin_types[next_smallest_coin_type_index..-1]
    denominations.append(*remaining_coin_types)
  end
  private_class_method :append_coin_type_subset
end
