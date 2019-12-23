const MAX_COLORS = 2
const RESISTOR_VALUES = Object.freeze({
  black: "0",
  brown: "1",
  red: "2",
  orange: "3",
  yellow: "4",
  green: "5",
  blue: "6",
  violet: "7",
  grey: "8",
  white: "9"
})

export const decodedValue = colors => {
  return parseInt(colors.slice(0, MAX_COLORS).reduce(addResistorValue, ""))
}

const addResistorValue = (acc, color) => {
  return acc + RESISTOR_VALUES[color]
}
