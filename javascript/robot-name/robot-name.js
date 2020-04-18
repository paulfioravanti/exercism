export class Robot {
  static releaseNames() {
    RobotNameGenerator.reset()
  }

  constructor() {
    this.reset()
  }

  get name() {
    return this._name
  }

  reset() {
    this._name = RobotNameGenerator.getName()
  }
}

class RobotNameGenerator {
  static ALPHABET = [...'ABCDEFGHIJKLMNOPQRSTUVWXYZ']
  static DIGITS = [...'0123456789']
  static POSSIBLE_NAMES = Object.freeze(
    RobotNameGenerator.cartesianProduct(
      RobotNameGenerator.ALPHABET,
      RobotNameGenerator.ALPHABET,
      RobotNameGenerator.DIGITS,
      RobotNameGenerator.DIGITS,
      RobotNameGenerator.DIGITS
    ).map(nameCombination => nameCombination.join(""))
  )
  static NAMES

  static reset() {
    RobotNameGenerator.NAMES =
      RobotNameGenerator.shuffle(RobotNameGenerator.POSSIBLE_NAMES.slice())
  }

  static getName() {
    if (RobotNameGenerator.NAMES.length === 0) {
      throw new Error()
    }

    return RobotNameGenerator.NAMES.pop()
  }

  static cartesianProduct(...data) {
    return (
      data.reduce(
        (a, b) => a.flatMap(x => b.map(y => [...x, y])),
        [[]]
      )
    )
  }

  // Fisher-Yates Shuffle. Shuffles array in place.
  static shuffle(array) {
    for (let i = array.length - 1; i > 0; i--) {
      const j = Math.floor(Math.random() * (i + 1));
      [array[i], array[j]] = [array[j], array[i]]
    }

    return array
  }
}

RobotNameGenerator.reset()
