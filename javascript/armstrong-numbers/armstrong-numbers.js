export const isArmstrongNumber = num => {
  const sumOfPowers = sumPowers(digits(num))

  return num === sumOfPowers
}

function digits(num) {
  return Array.from(num.toString()).map(Number)
}

function sumPowers(digits) {
  return digits.reduce(power(digits.length), 0)
}

function power(length) {
  return (acc, digit) => {
    return acc + Math.pow(digit, length)
  }
}
