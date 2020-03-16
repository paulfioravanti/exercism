type Grade = string
type StudentList = string[]
type RosterEntry = [Grade, StudentList]
type Roster = Map<Grade, StudentList>
type Maybe<T> = T | undefined

export default class GradeSchool {
  private readonly GRADE: number = 0
  private readonly STUDENT_LIST: number = 1
  private roster: Roster

  constructor() {
    this.roster = new Map()
  }

  studentRoster(): Roster {
    return this.clone(this.roster)
  }

  addStudent(name: string, gradeNum: number): void {
    const roster: Roster = this.roster
    const grade: Grade = gradeNum.toString()
    let students: Maybe<StudentList> = roster.get(grade)

    if (!students) {
      students = []
    }

    students.push(name)
    roster.set(grade, students.sort())
  }

  studentsInGrade(gradeNum: number): StudentList {
    const grade: Grade = gradeNum.toString()
    const students: Maybe<StudentList> = this.studentRoster().get(grade)

    return students ? students : []
  }

  private clone(roster: Roster): Roster {
    return new Map(
      [...roster.entries()].map(this.cloneRosterEntry.bind(this))
    )
  }

  private cloneRosterEntry(entry: RosterEntry): RosterEntry {
    return [
      entry[this.GRADE] as Grade,
      entry[this.STUDENT_LIST].slice() as StudentList
    ]
  }
}
