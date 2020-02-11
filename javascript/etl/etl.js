export const transform = scores => {
  return Object.entries(scores).reduce(transformLetters, {})
};

const transformLetters = (acc, [pointValue, letters]) => {
  return letters.reduce(assignPointValue(pointValue), acc)
}

const assignPointValue = (pointValue) => {
  return (acc, letter) => {
    acc[letter.toLowerCase()] = parseInt(pointValue)
    return acc
  }
}
