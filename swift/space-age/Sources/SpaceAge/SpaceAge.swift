struct SpaceAge {
  private enum Planet {
    case Earth
    case Mercury
    case Venus
    case Mars
    case Jupiter
    case Saturn
    case Uranus
    case Neptune
  }
  private static let EARTH_ORBITAL_PERIOD: Double = 31557600.0
  private static let EARTH_ORBITAL_FACTOR: Double = 1.0
  private static let ORBITAL_FACTORS: [Planet: Double] = [
    Planet.Earth: SpaceAge.EARTH_ORBITAL_FACTOR,
    Planet.Mercury: 0.2408467,
    Planet.Venus: 0.61519726,
    Planet.Mars: 1.8808158,
    Planet.Jupiter: 11.862615,
    Planet.Saturn: 29.447498,
    Planet.Uranus: 84.016846,
    Planet.Neptune: 164.79132
  ]

  let seconds: Int

  init(_ seconds: Int) {
    self.seconds = seconds
  }

  var onEarth: Double {
    return on(Planet.Earth)
  }

  var onMercury: Double {
    return on(Planet.Mercury)
  }

  var onVenus: Double {
    return on(Planet.Venus)
  }

  var onMars: Double {
    return on(Planet.Mars)
  }

  var onJupiter: Double {
    return on(Planet.Jupiter)
  }

  var onSaturn: Double {
    return on(Planet.Saturn)
  }

  var onUranus: Double {
    return on(Planet.Uranus)
  }

  var onNeptune: Double {
    return on(Planet.Neptune)
  }

  private func on(_ planet: Planet) -> Double {
    guard let orbitalFactor = SpaceAge.ORBITAL_FACTORS[planet] else {
      return SpaceAge.EARTH_ORBITAL_FACTOR
    }
    return Double(seconds) / SpaceAge.EARTH_ORBITAL_PERIOD / orbitalFactor
  }
}
