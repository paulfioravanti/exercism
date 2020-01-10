module Bob
  extend self

  struct Remark
    def initialize(@remark : String) : String
    end

    def silence? : Bool
      remark.blank?
    end

    def shouting_question? : Bool
      shouting? && question?
    end

    def shouting? : Bool
      remark == remark.upcase && remark != remark.downcase
    end

    def question? : Bool
      remark.ends_with?("?")
    end

    private getter remark : String
  end

  def hey(input : String) : String
    case Remark.new(input)
    when .silence?
      "Fine. Be that way!"
    when .shouting_question?
      "Calm down, I know what I'm doing!"
    when .question?
      "Sure."
    when .shouting?
      "Whoa, chill out!"
    else
      "Whatever."
    end
  end
end
