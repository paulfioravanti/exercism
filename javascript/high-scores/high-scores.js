export class HighScores {
  constructor(scores) {
    this._scores = scores
  }

  get scores() {
    return this._scores
  }

  get latest() {
    return this.scores.pop()
  }

  get personalBest() {
    return Math.max(...this.scores)
  }

  get personalTopThree() {
    return this.scores.sort(this.descending).slice(0, 3)
  }

  descending(a, b) {
    return b - a
  }
}
