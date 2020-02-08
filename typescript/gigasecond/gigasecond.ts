export default class Gigasecond {
  private readonly MILLISECONDS: number = 10**12
  private readonly startDate: Date

  constructor(startDate: Date) {
    this.startDate = startDate
  }

  date = (): Date => {
    return new Date(this.startDate.getTime() + this.MILLISECONDS)
  }
}
