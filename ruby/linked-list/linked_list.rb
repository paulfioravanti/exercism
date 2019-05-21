class Deque
  class Node
    attr_reader :value
    attr_accessor :prev, :succ

    def initialize(value, prev: self, succ: self)
      @value = value
      @prev = prev
      @succ = succ
    end
  end
  private_constant :Node

  def initialize
    @head = nil
  end

  def push(value)
    if head
      tail = head.prev
      node = Node.new(value, prev: tail, succ: head)
      tail.succ = node
      head.prev = node
    else
      self.head = Node.new(value)
    end
  end

  def pop
    if (tail = head.prev) == head
      self.head = nil
    else
      head.prev = tail.prev
      tail.prev.succ = head
    end
    tail.value
  end

  def shift
    if (node = head) == head.succ
      self.head = nil
    else
      self.head = node.succ
      head.prev = node.prev
      head.prev.succ = head
    end
    node.value
  end

  def unshift(value)
    if head
      node = Node.new(value, prev: head.prev, succ: head)
      head.prev = node
      self.head = node
    else
      self.head = Node.new(value)
    end
  end

  private

  attr_accessor :head
end
