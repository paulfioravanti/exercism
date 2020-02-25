const PHONE_NUMBER_LENGTH = 11
const VALID_COUNTRY_CODE = "1"
const NON_NUMBERS = /\D/g
const PATTERNS = Object.freeze([
  ["Letters", /[a-z]/ig],
  ["Punctuations", /[@:!]/g]
])
const VALID_NUMBER = new RegExp([
  "^",
  "\\+?1?",                      // optional country code
  "\\s*",                        // optional divider
  "\\(?",                        // optional left parenthesis
  "(?<areaCode>\\d{3})",         // area code
  "\\)?",                        // optional right parenthesis
  "[-\\.\\s]*",                  // optional divider
  "(?<exchangeCode>\\d{3})",     // exchange code
  "[-\\.\\s]*",                  // optional divider
  "(?<subscriberNumber>\\d{4})", // subscriber number
  "\\s*",
  "$"
].join(""))
const INVALID_FIRST_DIGITS = Object.freeze({
  "0": "zero",
  "1": "one"
})

export const clean = input => {
  validateInput(input)
  const phoneNumber = VALID_NUMBER.exec(input)
  const validatedPhoneNumber = validateNumber(phoneNumber)

  return validatedPhoneNumber
}

function validateInput(input) {
  PATTERNS.forEach(checkPattern(input))

  const numbers = input.replace(NON_NUMBERS, "")

  if (hasTooManyDigits(numbers)) {
    throw new Error("More than 11 digits")
  }

  if (hasIncorrectCountryCode(numbers)) {
    throw new Error("11 digits must start with 1")
  }
}

function checkPattern(input) {
  return ([patternType, regexp]) => {
    if (regexp.test(input)) {
      throw new Error(`${patternType} not permitted`)
    }
  }
}

function hasTooManyDigits(numbers) {
  return numbers.length > PHONE_NUMBER_LENGTH
}

function hasIncorrectCountryCode(numbers) {
  return (
    numbers.length === PHONE_NUMBER_LENGTH &&
    numbers[0] !== VALID_COUNTRY_CODE
  )
}

function validateNumber(phoneNumber) {
  if (!phoneNumber) {
    throw new Error("Incorrect number of digits")
  }

  /* eslint-disable no-unused-vars */
  const [_match, areaCode, exchangeCode, subscriberNumber] = phoneNumber
  /* eslint-enable no-unused-vars */

  validateFirstDigit("Area", areaCode)
  validateFirstDigit("Exchange", exchangeCode)

  return areaCode + exchangeCode + subscriberNumber
}

function validateFirstDigit(numberType, number) {
  const firstNumber = number[0]
  if (Object.keys(INVALID_FIRST_DIGITS).includes(firstNumber)) {
    const numberWord = INVALID_FIRST_DIGITS[firstNumber]
    throw new Error(`${numberType} code cannot start with ${numberWord}`)
  }
}
