export class GradeSchool {
  constructor() {
    this._roster = {}
  }

  roster() {
    return this.clone(this._roster)
  }

  add(name, grade) {
    const roster = this._roster
    let names = roster[grade]

    if (!names) {
      names = []
    }

    names.push(name)
    names.sort()
    roster[grade] = names
  }

  grade(grade) {
    const roster = this.roster()

    if (!roster || !roster[grade]) {
      return []
    }

    return roster[grade]
  }

  clone(object) {
    return JSON.parse(JSON.stringify(object))
  }
}
