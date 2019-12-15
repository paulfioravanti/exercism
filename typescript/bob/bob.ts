class Bob {
  private readonly LETTERS: RegExp = /[A-Za-z]/i

  hey(input: string): string {
    const remark: string = input.trim()

    if (this.isSilence(remark)) {
      return "Fine. Be that way!"
    } else {
      return this.respondToVerbalRemark(remark)
    }
  }

  private isSilence(remark: string): boolean {
    return remark === ""
  }

  private respondToVerbalRemark(remark: string): string {
    const shoutingQuestion: string = [
      this.isQuestion(remark),
      this.isShouting(remark)
    ].join()

    switch (shoutingQuestion) {
      case "true,true":
        return "Calm down, I know what I'm doing!"
      case "true,false":
        return "Sure."
      case "false,true":
        return "Whoa, chill out!"
      default:
        return "Whatever."
    }
  }

  private isQuestion(remark: string): boolean {
    return remark.endsWith("?")
  }

  private isShouting(remark: string): boolean {
    return this.hasLetters(remark) && remark.toUpperCase() === remark
  }

  private hasLetters(remark: string): boolean {
    return this.LETTERS.test(remark)
  }
}

export default Bob
