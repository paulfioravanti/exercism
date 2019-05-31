require "forwardable"

require "pry"
class Node
  attr_accessor :value, :left, :right

  def initialize(value, left, right)
    @value = value
    @left = left
    @right = right
  end

  def ==(other)
    value == other.value &&
      left == other.left &&
      right == other.right
  end
end

class Zipper
  extend Forwardable

  def self.from_tree(tree, focus = tree, parents = [])
    new(tree, focus, parents)
  end

  private_class_method :new

  def_delegators :@focus, :value, :nil?

  def initialize(tree, focus, parents)
    @tree = tree
    @focus = focus
    @parents = parents
  end

  def to_tree
    tree
  end

  def left
    self.class.from_tree(tree, focus.left, parents.push(focus))
  end

  def right
    self.class.from_tree(tree, focus.right, parents.push(focus))
  end

  def up
    self.class.from_tree(tree, parents.pop, parents)
  end

  # rubocop:disable Naming/AccessorMethodName
  def set_value(value)
    focus.value = value
    self
  end

  def set_left(node)
    focus.left = node
    self
  end

  def set_right(node)
    focus.right = node
    self
  end
  # rubocop:enable Naming/AccessorMethodName

  def ==(other)
    focus == other.focus &&
      tree == other.tree &&
      parents == other.parents
  end

  protected

  attr_reader :focus, :tree, :parents
end
