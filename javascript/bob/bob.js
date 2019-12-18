export const hey = message => {
  const remark = message.trim()

  if (isSilence(remark)) {
    return "Fine. Be that way!"
  } else {
    return respondToVerbalRemark(remark)
  }
}

function isSilence(remark) {
  return remark === ""
}

function respondToVerbalRemark(remark) {
  const question = isQuestion(remark)
  const shouting = isShouting(remark)

  if (question && shouting) {
    return "Calm down, I know what I'm doing!"
  } else if (question) {
    return "Sure."
  } else if (shouting) {
    return "Whoa, chill out!"
  } else {
    return "Whatever."
  }
}

function isQuestion(remark) {
  return remark.endsWith("?")
}

function isShouting(remark) {
  return hasLetters(remark) && remark.toUpperCase() === remark
}

function hasLetters(remark) {
  return LETTERS.test(remark)
}

const LETTERS = /[A-Za-z]/i
