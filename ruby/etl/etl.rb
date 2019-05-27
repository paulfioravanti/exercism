module ETL
  module_function

  def transform(scores)
    scores.each_with_object({}, &method(:transform_letters))
  end

  def transform_letters((point_value, letters), new_scores)
    letters.each do |letter|
      new_scores[letter.downcase] = point_value
    end
  end
  private_class_method :transform_letters
end
