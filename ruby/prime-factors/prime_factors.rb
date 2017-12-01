module PrimeFactors
  MINIMUM_PRIME = 2
  private_constant :MINIMUM_PRIME
  FINAL_FACTOR = 1
  private_constant :FINAL_FACTOR

  module_function

  def for(number)
    [].tap do |prime_factors|
      MINIMUM_PRIME.upto(number) do |n|
        quotient, modulus = number.divmod(n)
        next if modulus.nonzero?
        prime_factors << n
        break if quotient == FINAL_FACTOR
        number = quotient
        redo
      end
    end
  end
end
