module Commands

open Domain
open IO

let isNotExitCommand command = 
    match command with 
     | Exit -> false
     | _ -> true 

let executeSetCurrentDateCommand state (SetCurrentDateCommand date) =
    sendOutput (sprintf "Set current date %A" date)
    let dateTime = dateToDateTime date
    let newState = {state with selectedDate = Some dateTime}
    newState

let executeGetCurrentDateCommand state =
    match state.selectedDate with
     | None -> sendOutput("No date selected")
     | Some date -> sendOutput (sprintf "Current date: %s" (date.ToString("yyyy-MM-dd"))) 
    state

let handleCommand state command = 
    let newState = match command with
                    | SetCurrentDate command -> executeSetCurrentDateCommand state command
                    | GetCurrentDate -> executeGetCurrentDateCommand state
                    | Exit -> sendOutput "exit"; state
    
    newState
