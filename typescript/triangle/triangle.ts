type MaybeString = string | never
type TriangleSides = [number, number, number]

export default class Triangle {
  private readonly IS_EQUILATERAL = (uniqueSides: number[]): boolean => {
    return uniqueSides.length === 1
  }
  private readonly IS_ISOCELES = (uniqueSides: number[]): boolean => {
    return uniqueSides.length < 3
  }
  private readonly IS_SCALENE = (uniqueSides: number[]): boolean => {
    return uniqueSides.length === 3
  }
  private readonly sides: TriangleSides

  constructor(...sides: TriangleSides) {
    this.sides = sides.sort(this.ascending)
  }

  kind(): MaybeString {
    if (!this.allSidesLegal()) {
      throw new Error("Invalid triangle sides")
    }

    const uniqueSides = this.uniqueSides()

    if (this.IS_EQUILATERAL(uniqueSides)) {
      return "equilateral"
    } else if (this.IS_ISOCELES(uniqueSides)) {
      return "isosceles"
    } else if (this.IS_SCALENE(uniqueSides)) {
      return "scalene"
    } else {
      throw new Error("Unexpected triangle type")
    }
  }

  private ascending = (a: number, b: number): number => a - b

  private uniqueSides(): number[] {
    return [...new Set(this.sides)]
  }

  private allSidesLegal(): boolean {
    return this.allSidesPositive() && this.allSidesLegalLength()
  }

  private allSidesPositive(): boolean {
    return this.sides.every(side => side > 0)
  }

  private allSidesLegalLength(): boolean {
    const [firstSide, secondSide, thirdSide] = this.sides
    return firstSide + secondSide >= thirdSide
  }
}
