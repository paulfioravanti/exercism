module FlattenArray
  module_function

  def flatten(array)
    array.reduce([]) do |acc, element|
      case element
      when Array
        acc.concat(flatten(element))
      when nil
        acc
      else
        acc << element
      end
    end
  end
end
