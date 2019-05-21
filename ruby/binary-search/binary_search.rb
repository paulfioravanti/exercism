class BinarySearch
  INITIAL_INDEX = 0
  private_constant :INITIAL_INDEX
  MAX_INDEX = ->(list) { list.length - 1 }
  private_constant :INITIAL_INDEX
  MIDDLE_INDEX = ->(from, to) { from + (to - from) / 2 }
  private_constant :MIDDLE_INDEX

  attr_reader :list

  def initialize(list)
    raise ArgumentError unless list == list.sort

    @list = list
  end

  def search_for(element)
    raise RuntimeError unless list.include?(element)

    first_index = INITIAL_INDEX
    last_index = MAX_INDEX.call(list)
    search(element, first_index, last_index)
  end

  def middle(from: INITIAL_INDEX, to: list.length)
    MIDDLE_INDEX.call(from, to)
  end

  private

  def search(element, first_index, last_index)
    loop do
      midpoint = middle(from: first_index, to: last_index)
      case list[midpoint] <=> element
      when 1 then last_index = midpoint - 1
      when 0 then return midpoint
      when -1 then first_index = midpoint + 1
      end
    end
  end
end
