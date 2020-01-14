struct Bob {
  static func hey(_ remark: String) -> String {
    if isSilence(remark) {
      return "Fine. Be that way!"
    } else if isYelling(remark) {
      return "Whoa, chill out!"
    } else if isQuestion(remark) {
      return "Sure."
    } else {
      return "Whatever."
    }
  }

  private static func isSilence(_ remark: String) -> Bool {
    return remark.filter { !$0.isWhitespace }.isEmpty
  }

  private static func isQuestion(_ remark: String) -> Bool {
    return remark.hasSuffix("?")
  }

  private static func isYelling(_ remark: String) -> Bool {
    return remark == remark.uppercased() && remark != remark.lowercased()
  }
}
