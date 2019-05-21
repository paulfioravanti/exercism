class Deque
  class Node
    attr_reader :value
    attr_accessor :prev_node, :next_node

    def initialize(value, prev_node: self, next_node: self)
      @value = value
      @prev_node = prev_node
      @next_node = next_node
    end
  end
  private_constant :Node

  def initialize
    @head = nil
  end

  def push(value)
    if head
      tail = head.prev_node
      new_tail = Node.new(value, prev_node: tail, next_node: head)
      tail.next_node = new_tail
      head.prev_node = new_tail
    else
      self.head = Node.new(value)
    end
  end

  def pop
    if (tail = head.prev_node) == head
      self.head = nil
    else
      head.prev_node = tail.prev_node
      tail.prev_node.next_node = head
    end
    tail.value
  end

  def shift
    if (shifted_head = head) == head.next_node
      self.head = nil
    else
      self.head = shifted_head.next_node
      head.prev_node = shifted_head.prev_node
      head.prev_node.next_node = head
    end
    shifted_head.value
  end

  def unshift(value)
    if head
      new_head = Node.new(value, prev_node: head.prev_node, next_node: head)
      head.prev_node = new_head
      self.head = new_head
    else
      self.head = Node.new(value)
    end
  end

  private

  attr_accessor :head
end
