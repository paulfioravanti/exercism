type Planet =
  | "earth"
  | "mercury"
  | "venus"
  | "mars"
  | "jupiter"
  | "saturn"
  | "uranus"
  | "neptune"
type OrbitalFactors = Readonly<Record<Planet, number>>

export default class SpaceAge {
  private readonly EARTH_ORBITAL_PERIOD: Readonly<number> = 31557600.0
  private readonly ORBITAL_FACTORS: OrbitalFactors = Object.freeze({
    earth: 1,
    mercury: 0.2408467,
    venus: 0.61519726,
    mars: 1.8808158,
    jupiter: 11.862615,
    saturn: 29.447498,
    uranus: 84.016846,
    neptune: 164.79132
  })

  readonly seconds: Readonly<number>

  constructor(seconds: number) {
    this.seconds = seconds
  }

  onEarth(): number {
    return this.spaceAge("earth")
  }

  onMercury(): number {
    return this.spaceAge("mercury")
  }

  onVenus(): number {
    return this.spaceAge("venus")
  }

  onMars(): number {
    return this.spaceAge("mars")
  }

  onJupiter(): number {
    return this.spaceAge("jupiter")
  }

  onSaturn(): number {
    return this.spaceAge("saturn")
  }

  onUranus(): number {
    return this.spaceAge("uranus")
  }

  onNeptune(): number {
    return this.spaceAge("neptune")
  }

  private spaceAge(planet: Planet): number {
    return Number(
      (
        this.seconds /
        this.EARTH_ORBITAL_PERIOD /
        this.ORBITAL_FACTORS[planet]
      ).toFixed(2)
    )
  }
}
