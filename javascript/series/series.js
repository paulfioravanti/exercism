export class Series {
  static get STEP() {
    return 1
  }

  constructor(input) {
    this._digits = input.split("").map(Number)
  }

  get digits() {
    return this._digits
  }

  slices(size) {
    const digits = this.digits
    if (size > digits.length) {
      throw new Error("Slice size is too big.")
    }

    return this.chunkEvery(size, digits)
  }

  chunkEvery(size, digits) {
    if (size === digits.length) {
      return [digits]
    }

    const slice = digits.slice(0, size)
    digits.splice(0, Series.STEP)
    return [slice].concat(this.chunkEvery(size, digits))
  }
}
