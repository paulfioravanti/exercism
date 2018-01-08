class SimpleLinkedList
  class Element
    attr_reader :datum
    attr_accessor :next

    def initialize(datum)
      @datum = datum
    end
  end
  private_constant :Element

  include Enumerable

  def initialize(data = [])
    data.each { |datum| push(Element.new(datum)) }
  end

  def each
    # Enumerable#entries
    return enum_for(:each) { entries.size } unless block_given?

    element = head
    while element
      yield element.datum
      element = element.next
    end
  end

  def push(element)
    tap do
      element.next = head
      self.head = element
    end
  end

  def pop
    head.tap do |element|
      self.head = element&.next
    end
  end

  def reverse
    # Enumerable#entries
    self.class.new(entries)
  end

  def reverse!
    tap do
      next_element = head
      self.head = nil
      while (element = next_element)
        next_element = element.next
        push(element)
      end
    end
  end

  private

  attr_accessor :head
end

# Break encapsulation for the tests.
Element = SimpleLinkedList.const_get(:Element)

module BookKeeping
  VERSION = 1
end
