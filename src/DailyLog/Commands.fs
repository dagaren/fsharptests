module Commands

open Domain
open IO
open System

type RelativeDate =
 | Date of Date
 | Yesterday
 | Today

type SetCurrentDateCommand = SetCurrentDateCommand of RelativeDate

type AddLogCommand = AddLogCommand of string

type Command =
 | SetCurrentDate of SetCurrentDateCommand
 | GetCurrentDate
 | ShowLogs
 | AddLog of AddLogCommand
 | Exit

type AppState = {
    SelectedDate: Date option
    Logs: Map<Date, LogEntry list>
}

let initialState = {
    SelectedDate = None
    Logs = Map.empty<Date, LogEntry list>
}

let relativeDateToDateTime (date:RelativeDate) =
    match date with
     | Date date -> DateTime(date.year, date.month, date.day)
     | Yesterday -> DateTime.UtcNow.AddDays(-1.0)
     | Today -> DateTime.UtcNow

let dateTimeToDate (date:DateTime) =
    { year = date.Year; month = date.Month; day = date.Day }

let isExitCommand command = 
    match command with 
     | Exit -> true
     | _ -> false

let executeSetCurrentDateCommand state (SetCurrentDateCommand relativeDate) =
    let date = (relativeDateToDateTime >> dateTimeToDate) relativeDate
    sendOutput (sprintf "Set current date %A" date)
    let newState = {state with SelectedDate = Some date}
    newState

let executeGetCurrentDateCommand state =
    match state.SelectedDate with
     | None -> sendOutput("No date selected")
     | Some date -> sendOutput (sprintf "Current date: %s" (dateToString date)) 
    state

let showLogs (date:Date) (logs:LogEntry list) =
    let header = sprintf "%s:\n" (dateToString date)
    let body = logs |> List.fold (fun acc (LogEntry logText) -> acc + (sprintf "* %s\n" logText)) header 
    sendOutput(body)

let executeShowLogsCommand state =
    match state.SelectedDate with
     | None -> sendOutput "No date selected"
     | Some date -> 
        let dateLogs = state.Logs.TryFind date
        match dateLogs with 
         | None -> sendOutput "No logs for selected date"
         | Some logs -> showLogs date logs
    state

let executeAddLogCommand state (AddLogCommand text) =
    match state.SelectedDate with
     | None ->
        sendOutput "No date selected"
        state
     | Some date -> 
        let newLogs = state.Logs |> Map.change date (fun current ->
            match current with
             | None -> Some [ (LogEntry text) ]
             | Some currentLogs ->
                let newLogs = currentLogs |> List.append [(LogEntry text)] 
                Some newLogs)
        {state with Logs = newLogs}

let handleCommand state command = 
    let newState = match command with
                    | SetCurrentDate command -> executeSetCurrentDateCommand state command
                    | GetCurrentDate -> executeGetCurrentDateCommand state
                    | ShowLogs -> executeShowLogsCommand state
                    | AddLog command -> executeAddLogCommand state command
                    | Exit -> sendOutput "exit"; state
    
    newState
