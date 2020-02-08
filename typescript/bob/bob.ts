export default class Bob {
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
    const isQuestion: boolean = this.isQuestion(remark)
    const isShouting: boolean = this.isShouting(remark)

    if (isQuestion && isShouting) {
      return "Calm down, I know what I'm doing!"
    } else if (isQuestion) {
      return "Sure."
    } else if (isShouting) {
      return "Whoa, chill out!"
    } else {
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
