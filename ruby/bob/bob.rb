# frozen_string_literal: true

module Bob
  module_function

  def hey(input)
    if (remark = input.strip).blank?
      return "Fine. Be that way!"
    end

    respond_to_verbal_remark(remark)
  end

  private_class_method def respond_to_verbal_remark(remark)
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

  private_class_method def question?(remark)
    remark.end_with?("?")
  end

  private_class_method def shouting?(remark)
    remark == remark.upcase && remark != remark.downcase
  end
end
