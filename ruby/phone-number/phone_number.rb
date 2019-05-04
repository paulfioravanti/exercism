# frozen_string_literal: true

module PhoneNumber
  BLANK = ""
  private_constant :BLANK
  COUNTRY_CODE_AND_NON_DIGITS = /\A\+?1|\D/.freeze
  private_constant :COUNTRY_CODE_AND_NON_DIGITS
  VALID_NUMBER =
    /\A
     \+?1?\s*                 # country code
     \(?[2-9]\d{2}\)?[-\.\s]* # area code
     [2-9]\d{2}[-\.\s]*       # exchange code
     \d{4}\s*                 # subscriber number
    \z/x.freeze
  private_constant :VALID_NUMBER

  module_function

  def clean(phone_number)
    return nil unless phone_number.match?(VALID_NUMBER)

    phone_number.gsub(COUNTRY_CODE_AND_NON_DIGITS, BLANK)
  end
end
