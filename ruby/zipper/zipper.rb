require "forwardable"

Node = Struct.new(:value, :left, :right)

class Zipper
  extend Forwardable

  def self.from_tree(tree, focus = tree, parents = [])
    new(tree, focus, parents)
  end

  private_class_method :new

  def_delegators :@focus, :value, :value=, :nil?
  def_delegator :self, :tree, :to_tree
  def_delegator self, :from_tree

  def initialize(tree, focus, parents)
    @tree = tree
    @focus = focus
    @parents = parents
  end

  def left
    from_tree(tree, focus.left, parents.push(focus))
  end

  def right
    from_tree(tree, focus.right, parents.push(focus))
  end

  def up
    from_tree(tree, parents.pop, parents)
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
    protected_methods
      .each
      .with_object(other)
      .all?(&method(:equal_to_other?))
  end

  protected

  attr_reader :focus, :tree, :parents

  private

  def equal_to_other?(method, other)
    send(method) == other.send(method)
  end
end
