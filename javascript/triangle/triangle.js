export class Triangle {
  constructor(...sides) {
    this.sides = sides.sort()
  }

  isEquilateral() {
    return this.checkAllSidesLegal() && this.uniqueSides().length === 1
  }

  isIsosceles() {
    return this.checkAllSidesLegal() && this.uniqueSides().length < 3
  }

  isScalene() {
    return this.checkAllSidesLegal() && this.uniqueSides().length === 3
  }

  // Internal

  uniqueSides() {
    return [...new Set(this.sides)]
  }

  checkAllSidesLegal() {
    return this.checkAllSidesPositive() && this.checkAllSidesLegalLength()
  }

  checkAllSidesPositive() {
    return this.sides.every(side => side > 0)
  }

  checkAllSidesLegalLength() {
    const [firstSide, secondSide, thirdSide] = this.sides
    return firstSide + secondSide >= thirdSide
  }
}
