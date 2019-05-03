# frozen_string_literal: true

module Bob
  DEFAULT_RESPONSE = "Whatever."
  private_constant :DEFAULT_RESPONSE
  QUESTION_MARK = "?"
  private_constant :QUESTION_MARK
  QUESTION_RESPONSE = "Sure."
  private_constant :QUESTION_RESPONSE
  SHOUTING_QUESTION_RESPONSE = "Calm down, I know what I'm doing!"
  private_constant :SHOUTING_QUESTION_RESPONSE
  SHOUTING_RESPONSE = "Whoa, chill out!"
  private_constant :SHOUTING_RESPONSE
  SILENCE_RESPONSE = "Fine. Be that way!"
  private_constant :SILENCE_RESPONSE

  module_function

  def hey(input)
    remark = input.strip

    if silence?(remark)
      SILENCE_RESPONSE
    else
      respond_to_verbal_remark(remark)
    end
  end

  def respond_to_verbal_remark(remark)
    case [question?(remark), shouting?(remark)]
    when [true, true]
      SHOUTING_QUESTION_RESPONSE
    when [true, false]
      QUESTION_RESPONSE
    when [false, true]
      SHOUTING_RESPONSE
    else
      DEFAULT_RESPONSE
    end
  end
  private_class_method :respond_to_verbal_remark

  def silence?(remark)
    remark.empty?
  end
  private_class_method :silence?

  def shouting?(remark)
    remark == remark.upcase && remark != remark.downcase
  end
  private_class_method :shouting?

  def question?(remark)
    remark.end_with?(QUESTION_MARK)
  end
  private_class_method :question?
end
