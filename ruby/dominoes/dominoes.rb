require "pry"
module Dominoes
  PAIR = 2
  private_constant :PAIR

  module_function

  # def chain?(dominoes)
  #   eulerian_cycle?(dominoes) && connected?(dominoes)
  # end

  # # https://en.wikipedia.org/wiki/Eulerian_path
  # def eulerian_cycle?(dominoes)
  #   dominoes
  #     .flatten
  #     .group_by(&:itself)
  #     .values
  #     .map(&:length)
  #     .all?(&:even?)
  # end

  # def connected?(dominoes)
  #   return true if dominoes.empty?

  #   head, *tail = dominoes
  #   loop do
  #     intersections, tail = tail.partition { |edge| intersects?(edge, head) }
  #     return true if tail.empty?
  #     return false if intersections.empty? && tail.any?

  #     head.append(*intersections.flatten)
  #   end
  # end

  # def intersects?(edge, head)
  #   (edge & head).any?
  # end

  def chain?(dominoes)
    return true if dominoes.empty?
    return adjacent?(dominoes.first) if dominoes.one?

    head, *tail = dominoes
    result =
      tail
      .each_with_object([head]) do |domino, acc|
        if adjacent?(acc.last, domino)
          acc.append(domino)
        elsif adjacent?(acc.last, flipped_domino = domino.reverse)
          acc.append(flipped_domino)
        elsif adjacent?(domino, acc.first)
          acc.prepend(domino)
        elsif adjacent?(flipped_domino = domino.reverse, acc.first)
          acc.prepend(flipped_domino)
        elsif adjacent?(acc.last, acc.first) && acc.length != dominoes.length
          next if adjacent?(domino)

          tail.push(*acc)
          acc.clear
          acc.push(domino)
        else
          tail.push(domino)
        end
      end

    return false if result.one? || result.length != dominoes.length

    domino_chain?(result)
  end

  def domino_chain?(dominoes)
    head, *_body, tail = dominoes
    adjacent?(tail, head) &&
      dominoes.each_cons(PAIR).all? { |d1, d2| adjacent?(d1, d2) }
  end
  private_class_method :domino_chain?

  def adjacent?(left, right = left)
    left.last == right.first
  end
  private_class_method :adjacent?
end