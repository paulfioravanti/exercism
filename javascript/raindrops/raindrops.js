const CONVERSIONS = Object.freeze([
  [3, "Pling"],
  [5, "Plang"],
  [7, "Plong"]
])

export const convert = number => {
  const raindrops = CONVERSIONS.reduce(addRaindrop(number), "")
  return raindrops === "" ? number.toString() : raindrops
}

function addRaindrop(number) {
  return (acc, [factor, raindrop]) => {
    return number % factor === 0 ? acc + raindrop : acc
  }
}
