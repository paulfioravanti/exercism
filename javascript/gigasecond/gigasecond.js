const MILLISECONDS = 10**12

export const gigasecond = date => {
  return new Date(date.getTime() + MILLISECONDS)
}
