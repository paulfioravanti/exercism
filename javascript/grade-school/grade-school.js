export class GradeSchool {
  constructor() {
    this._roster = {}
  }

  roster() {
    return this.clone(this.sortByGrades(this._roster))
  }

  add(name, grade) {
    const roster = this._roster
    const names = roster[grade]

    if (!names) {
      roster[grade] = [name]
    } else {
      names.push(name)
      names.sort()
      roster[grade] = names
    }
  }

  grade(grade) {
    const roster = this.roster()

    if (!roster || !roster[grade]) {
      return []
    }

    return roster[grade]
  }

  sortByGrades(roster) {
    return Object.keys(roster).sort().reduce(this.assignGrades(roster), {})
  }

  assignGrades(roster) {
    return (acc, grade) => {
      acc[grade] = roster[grade]
      return acc
    }
  }

  clone(object) {
    return JSON.parse(JSON.stringify(object))
  }
}
