module Bob
  extend self

  def hey(remark : String) : String
    if silence?(remark)
      "Fine. Be that way!"
    else
      respond_to_verbal_remark(remark)
    end
  end

  private def silence?(remark : String) : Bool
    remark.blank?
  end

  private def respond_to_verbal_remark(remark : String) : String
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

  private def shouting?(remark : String) : Bool
    remark == remark.upcase && remark != remark.downcase
  end

  private def question?(remark : String) : Bool
    remark.ends_with?("?")
  end
end
