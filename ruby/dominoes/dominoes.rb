require "pry"
module Dominoes
  module_function

  def chain?(dominoes)
    return true if dominoes.empty?

    if dominoes.one?
      return true if single_chainable?(dominoes)

      return false
    end

    head, *tail = dominoes
    result =
      tail
      .each_with_object([head]) do |candidate, acc|
        candidate_left, candidate_right = candidate
        first_left, _first_right = acc.first
        _last_left, last_right = acc.last
        # binding.pry
        if last_right == candidate_left
          acc.append(candidate)
        elsif first_left == candidate_right
          acc.prepend(candidate)
        elsif first_left == candidate_left
          acc.prepend(candidate.reverse)
        else
          tail << candidate
        end
      end
      .sort
    domino_chain?(result)
  end

  def single_chainable?(dominoes)
    dominoes
      .first
      .uniq
      .one?
  end
  private_class_method :single_chainable?

  def domino_chain?(dominoes)
    head, *_body, tail = dominoes
    has_adjacent_values?([tail, head]) &&
      dominoes.each_cons(2).all?(&method(:has_adjacent_values?))
  end
  private_class_method :domino_chain?

  def has_adjacent_values?((left, right))
    left.last == right.first
  end
  private_class_method :has_adjacent_values?
end
