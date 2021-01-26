module Domain

open System

type Date = 
 | Date of year:int * month:int * day:int
 | Yesterday
 | Today

type SetCurrentDateCommand = SetCurrentDateCommand of Date

type Command =
 | SetCurrentDate of SetCurrentDateCommand
 | GetCurrentDate
 | Exit

type AppState = {
    selectedDate: DateTime option
}

let initialState = {
    selectedDate = None
}

let dateToDateTime (date:Date) =
    match date with
     | Date (year, month, day) -> DateTime(year, month, day)
     | Yesterday -> DateTime.UtcNow.AddDays(-1.0)
     | Today -> DateTime.UtcNow