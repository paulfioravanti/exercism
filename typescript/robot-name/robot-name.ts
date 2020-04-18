type MaybeError<T> = T | never
type Maybe<T> = T | undefined

abstract class RobotNameGenerator {
  private static readonly ALPHABET: string[] = [...'ABCDEFGHIJKLMNOPQRSTUVWXYZ']
  private static readonly DIGITS: string[] = [...'0123456789']
  private static readonly POSSIBLE_NAMES = Object.freeze(
    RobotNameGenerator.cartesianProduct(
      RobotNameGenerator.ALPHABET,
      RobotNameGenerator.ALPHABET,
      RobotNameGenerator.DIGITS,
      RobotNameGenerator.DIGITS,
      RobotNameGenerator.DIGITS
    ).map((nameCombination: string[]) => nameCombination.join(""))
  )
  private static readonly NAMES: string[] =
      RobotNameGenerator.shuffle(RobotNameGenerator.POSSIBLE_NAMES.slice())

  static getName(): MaybeError<string> {
    const name: Maybe<string> = RobotNameGenerator.NAMES.pop()
    if (!name) {
      throw new Error()
    }

    return name
  }

  private static cartesianProduct(...data: Array<string[]>): Array<string[]> {
    return (
      data.reduce(
        (a: Array<string[]>, b: string[]) =>
          a.flatMap((x: string[]) =>
            b.map((y: string) => [...x, y])),
        [[]]
      )
    )
  }

  // Fisher-Yates Shuffle. Shuffles array in place.
  private static shuffle(array: string[]): string[] {
    for (let i: number = array.length - 1; i > 0; i--) {
      const j: number = Math.floor(Math.random() * (i + 1));
      [array[i], array[j]] = [array[j], array[i]]
    }

    return array
  }
}

export default class Robot {
  name: string

  constructor() {
    this.name = RobotNameGenerator.getName()
  }

  resetName(): void {
    this.name = RobotNameGenerator.getName()
  }
}
