module Dominoes
  module_function

  def chain?(dominoes)
    return true if dominoes.empty?

    eulerian_cycle?(dominoes) && connected_graph?(dominoes)
  end

  # https://en.wikipedia.org/wiki/Eulerian_path
  def eulerian_cycle?(dominoes)
    dominoes
      .flatten
      .group_by(&:itself)
      .values
      .map(&:length)
      .all?(&:even?)
  end
  private_class_method :eulerian_cycle?

  def connected_graph?(dominoes)
    head, *tail = dominoes
    loop do
      # tail represents a set of disjoints
      # https://en.wikipedia.org/wiki/Intersection_(set_theory)#Intersecting_and_disjoint_sets
      intersections, tail = tail.partition { |edge| intersects?(edge, head) }
      return true if tail.empty?
      return false if intersections.empty?

      head.append(*intersections.flatten)
    end
  end
  private_class_method :connected_graph?

  def intersects?(edge, head)
    (edge & head).any?
  end
  private_class_method :intersects?
end
