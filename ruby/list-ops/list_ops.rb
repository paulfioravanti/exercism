module ListOps
  ADJUSTMENT = 1
  private_constant :ADJUSTMENT
  INITIAL_ARRAY = [].freeze
  private_constant :INITIAL_ARRAY
  INITIAL_FACTORIAL = 1
  private_constant :INITIAL_FACTORIAL
  ZERO = 0
  private_constant :ZERO

  module_function

  # rubocop:disable Style/For
  def arrays(array)
    length = ZERO
    for _ in array
      length += ADJUSTMENT
    end
    length
  end
  # rubocop:enable Style/For

  def reverser(array)
    accumulator = INITIAL_ARRAY.dup
    index = arrays(array) - ADJUSTMENT

    while index >= ZERO
      accumulator << array[index]
      index -= ADJUSTMENT
    end
    accumulator
  end

  def concatter(array1, array2)
    reducer(
      array2,
      array1,
      ->(accumulator, element) { accumulator << element }
    )
  end

  def mapper(array)
    return array unless block_given?

    reducer(
      array,
      INITIAL_ARRAY.dup,
      ->(accumulator, element) { accumulator << yield(element) }
    )
  end

  def filterer(array)
    return array unless block_given?

    reducer(
      array,
      INITIAL_ARRAY.dup,
      lambda do |accumulator, element|
        accumulator.tap { |acc| acc << element if yield(element) }
      end
    )
  end

  def sum_reducer(array)
    reducer(
      array,
      ZERO,
      ->(accumulator, element) { accumulator + element }
    )
  end

  def factorial_reducer(array)
    reducer(
      array,
      INITIAL_FACTORIAL,
      ->(accumulator, element) { accumulator * element }
    )
  end

  # rubocop:disable Style/For
  def reducer(array, accumulator, function)
    for element in array
      accumulator = function.call(accumulator, element)
    end
    accumulator
  end
  # rubocop:enable Style/For
  private_class_method :reducer
end
