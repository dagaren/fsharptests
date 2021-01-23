module Domain

open System

type Date = 
 | Date of year:int * month:int * day:int
 | Yesterday
 | Today

type AddWordCommand = AddWordCommand of word:string
type RemoveWordCommand = RemoveWordCommand of word:string
type SetCurrentDateCommand = SetCurrentDateCommand of Date

type Command =
 | SetCurrentDate of SetCurrentDateCommand
 | GetCurrentDate
 | AddWord of AddWordCommand
 | RemoveWord of RemoveWordCommand
 | Exit

type AppState = {
    selectedDate: DateTime option
    words: string list
}

let initialState = {
    words = []
    selectedDate = None
}

let dateToDateTime (date:Date) =
    match date with
     | Date (year, month, day) -> DateTime(year, month, day)
     | Yesterday -> DateTime.UtcNow.AddDays(-1.0)
     | Today -> DateTime.UtcNow