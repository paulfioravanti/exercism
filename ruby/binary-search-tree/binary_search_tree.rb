# frozen_string_literal: true

class Bst
  attr_reader :data, :left, :right

  def initialize(data)
    @data = data
  end

  def insert(value)
    if data >= value
      branch_insert("left", value)
    else
      branch_insert("right", value)
    end
  end

  def each(&block)
    return enum_for(:each) unless block_given?

    left&.each(&block)
    yield(data)
    right&.each(&block)
  end

  private

  attr_writer :left, :right

  def branch_insert(branch, value)
    if (node = public_send(branch))
      node.insert(value)
    else
      send("#{branch}=", self.class.new(value))
    end
  end
end
