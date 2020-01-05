from typing import Dict
from enum import Enum, auto


class SpaceAge:
    class Planet(Enum):
        MERCURY = auto()
        VENUS = auto()
        EARTH = auto()
        MARS = auto()
        JUPITER = auto()
        SATURN = auto()
        URANUS = auto()
        NEPTUNE = auto()

    __EARTH_ORBITAL_PERIOD: float = 31557600.0
    __ORBITAL_FACTORS: Dict[Planet, float] = {
        Planet.EARTH: 1,
        Planet.MERCURY: 0.2408467,
        Planet.VENUS: 0.61519726,
        Planet.MARS: 1.8808158,
        Planet.JUPITER: 11.862615,
        Planet.SATURN: 29.447498,
        Planet.URANUS: 84.016846,
        Planet.NEPTUNE: 164.79132
    }

    def __init__(self, seconds: int) -> None:
        self.seconds = seconds

    def on_earth(self) -> float:
        return self.__on(self.Planet.EARTH)

    def on_mercury(self) -> float:
        return self.__on(self.Planet.MERCURY)

    def on_venus(self) -> float:
        return self.__on(self.Planet.VENUS)

    def on_mars(self) -> float:
        return self.__on(self.Planet.MARS)

    def on_jupiter(self) -> float:
        return self.__on(self.Planet.JUPITER)

    def on_saturn(self) -> float:
        return self.__on(self.Planet.SATURN)

    def on_uranus(self) -> float:
        return self.__on(self.Planet.URANUS)

    def on_neptune(self) -> float:
        return self.__on(self.Planet.NEPTUNE)

    def __on(self, planet: Planet) -> float:
        return round(
            self.seconds /
            self.__EARTH_ORBITAL_PERIOD /
            self.__ORBITAL_FACTORS[planet],
            2
        )
