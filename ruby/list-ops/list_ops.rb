module ListOps
  INCREMENT = 1
  private_constant :INCREMENT
  INITIAL_ARRAY = [].freeze
  private_constant :INITIAL_ARRAY
  INITIAL_FACTORIAL = 1
  private_constant :INITIAL_FACTORIAL
  ZERO = 0
  private_constant :ZERO

  module_function

  def arrays(array)
    reducer(
      array,
      ZERO,
      ->(accumulator, _element) { accumulator + INCREMENT }
    )
  end

  def reverser(array)
    reducer(
      array,
      INITIAL_ARRAY.dup,
      ->(accumulator, element) { accumulator.prepend(element) }
    )
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
  def reducer(collection, accumulator, function)
    for element in collection
      accumulator = function.call(accumulator, element)
    end
    accumulator
  end
  # rubocop:enable Style/For
  private_class_method :reducer
end
