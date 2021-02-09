module Domain

open System

type LogEntry = LogEntry of text:string

type Date = {
    year: int
    month: int
    day: int
}

type DailyLog = {
    date: Date
    entry: LogEntry
}

let dateToString date = 
    sprintf "%s %s %s" (date.year.ToString()) (date.month.ToString()) (date.day.ToString())