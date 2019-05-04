module ETL
  module_function

  def transform(scores)
    scores.each_with_object({}) do |(point_value, letters), new_scores|
      letters.each do |letter|
        new_scores[letter.downcase] = point_value
      end
    end
  end
end
