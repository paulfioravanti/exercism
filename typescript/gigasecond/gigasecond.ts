export default class Gigasecond {
  private readonly MILLISECONDS: number = 10**12
  private readonly fromDate: Date

  constructor(fromDate: Date) {
    this.fromDate = fromDate
  }

  date = (): Date => {
    return new Date(this.fromDate.getTime() + this.MILLISECONDS)
  }
}
