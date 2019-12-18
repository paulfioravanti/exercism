export const age = (planet, seconds) => {
  return parseFloat(
    (seconds / EARTH_ORBITAL_PERIOD / ORBITAL_FACTORS[planet]).toFixed(2)
  )
}

const EARTH_ORBITAL_PERIOD = 31557600.0
const ORBITAL_FACTORS = Object.freeze({
  earth: 1,
  mercury: 0.2408467,
  venus: 0.61519726,
  mars: 1.8808158,
  jupiter: 11.862615,
  saturn: 29.447498,
  uranus: 84.016846,
  neptune: 164.79132
})
