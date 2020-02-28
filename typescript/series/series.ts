type MaybeError<T> = T | never
type Slices = Array<number[]>

export default class Series {
  private readonly STEP: number = 1
  readonly digits: number[]

  constructor(input: string) {
    this.digits = input.split("").map(Number)
  }

  slices(size: number): MaybeError<Slices> {
    const digits: number[] = this.digits
    if (size > digits.length) {
      throw new Error("Slice size is too big.")
    }

    return this.chunkEvery(size, digits)
  }

  chunkEvery(size: number, digits: number[]): Slices {
    if (size === digits.length) {
      return [digits]
    }

    const slice: number[] = digits.slice(0, size)
    digits.splice(0, this.STEP)
    return [slice].concat(this.chunkEvery(size, digits))
  }
}
