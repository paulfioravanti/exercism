class Array
  def keep
    apply_predicate_to_collection do |element, collection|
      collection << element if yield(element)
    end
  end

  def discard
    apply_predicate_to_collection do |element, collection|
      collection << element unless yield(element)
    end
  end

  private

  def apply_predicate_to_collection
    each_with_object([]) do |element, collection|
      yield(element, collection)
    end
  end
end
