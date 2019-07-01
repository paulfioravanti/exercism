module GradeSchool exposing (addStudent, allStudents, empty, studentsInGrade)

import Dict exposing (Dict)


type alias Grade =
    Int


type alias Student =
    String


type alias School =
    Dict Grade (List Student)


empty : School
empty =
    Dict.empty


addStudent : Grade -> Student -> School -> School
addStudent grade student school =
    let
        addName : Maybe (List Student) -> Maybe (List Student)
        addName names =
            case names of
                Just students ->
                    Just (student :: students)

                Nothing ->
                    Just [ student ]
    in
    school
        |> Dict.update grade addName


studentsInGrade : Grade -> School -> List Student
studentsInGrade grade school =
    school
        |> Dict.get grade
        |> Maybe.withDefault []
        |> List.sort


allStudents : School -> List ( Grade, List Student )
allStudents school =
    Dict.toList school
