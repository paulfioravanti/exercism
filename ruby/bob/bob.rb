module Bob
  QUESTION_MARK = "?".freeze
  private_constant :QUESTION_MARK
  SHOUTING_LETTERS = /[A-Z]+/
  private_constant :SHOUTING_LETTERS
  SILENCE_RESPONSE = "Fine. Be that way!".freeze
  private_constant :SILENCE_RESPONSE
  SHOUTING_RESPONSE = "Whoa, chill out!".freeze
  private_constant :SHOUTING_RESPONSE
  QUESTION_RESPONSE = "Sure.".freeze
  private_constant :QUESTION_RESPONSE
  DEFAULT_RESPONSE = "Whatever.".freeze
  private_constant :DEFAULT_RESPONSE

  module_function

  def hey(message)
    message = message.strip
    if silence?(message)
      SILENCE_RESPONSE
    elsif shouting?(message)
      SHOUTING_RESPONSE
    elsif question?(message)
      QUESTION_RESPONSE
    else
      DEFAULT_RESPONSE
    end
  end

  def silence?(message)
    message.empty?
  end
  private_class_method :silence?

  def shouting?(message)
    message.match?(SHOUTING_LETTERS) && message == message.upcase
  end
  private_class_method :shouting?

  def question?(message)
    message.end_with?(QUESTION_MARK)
  end
  private_class_method :question?
end

module BookKeeping
  VERSION = 1
end
