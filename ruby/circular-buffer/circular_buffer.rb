require "forwardable"

class CircularBuffer
  class BufferEmptyException < StandardError; end
  class BufferFullException < StandardError; end

  extend Forwardable

  def initialize(size)
    @buffer = []
    @size = size
  end

  def_delegators :@buffer, :clear, :empty?, :length, :push, :shift

  def read
    raise BufferEmptyException if empty?

    shift
  end

  def write(char)
    raise BufferFullException if full?

    push(char)
  end

  def write!(char)
    read if full?
    write(char)
  end

  private

  attr_reader :size

  def full?
    length == size
  end
end
