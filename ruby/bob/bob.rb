# frozen_string_literal: true

module Bob
  module_function

  def hey(input)
    case input.strip
    in ""
      "Fine. Be that way!"
    in remark
      respond_to_verbal_remark(remark)
    end
  end

  def respond_to_verbal_remark(remark)
    case [question?(remark), shouting?(remark)]
    when [true, true]
      "Calm down, I know what I'm doing!"
    when [true, false]
      "Sure."
    when [false, true]
      "Whoa, chill out!"
    else
      "Whatever."
    end
  end
  private_class_method :respond_to_verbal_remark

  def question?(remark)
    remark.end_with?("?")
  end
  private_class_method :question?

  def shouting?(remark)
    remark == remark.upcase && remark != remark.downcase
  end
  private_class_method :shouting?
end
