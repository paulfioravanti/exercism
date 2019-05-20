require "forwardable"

class CustomSet
  include Comparable
  extend Forwardable

  def initialize(array)
    @array = array.sort
  end

  def_delegators :@array, :empty?, :include?, :member?, :push

  def ==(other)
    array == other.array
  end

  def subset?(other)
    difference(other).empty?
  end

  def disjoint?(other)
    intersection(other).empty?
  end

  def add(element)
    push(element).sort! unless include?(element)
    self
  end

  def intersection(other)
    self.class.new(self & other)
  end

  def difference(other)
    self.class.new(self - other)
  end

  def union(other)
    self.class.new(self | other)
  end

  protected

  attr_reader :array

  def &(other)
    array & other.array
  end

  def -(other)
    array - other.array
  end

  def |(other)
    array | other.array
  end
end
