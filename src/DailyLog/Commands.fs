module Commands

open Domain
open IO

let isNotExitCommand command = 
    match command with 
     | Exit -> false
     | _ -> true 

let executeAddWordCommand state (AddWordCommand word) = 
    let newWords = word :: state.words
    let newState = {state with words = newWords }
    sendOutput (sprintf "%A" newState.words)
    newState

let executeRemoveWordCommand state (RemoveWordCommand word) =
    let newWords = state.words |> List.filter (fun x -> x <> word)
    let newState = {state with words = newWords}
    sendOutput (sprintf "%A" newState.words)
    newState

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
                    | AddWord command -> executeAddWordCommand state command
                    | RemoveWord command -> executeRemoveWordCommand state command
                    | SetCurrentDate command -> executeSetCurrentDateCommand state command
                    | GetCurrentDate -> executeGetCurrentDateCommand state
                    | Exit -> sendOutput "exit"; state
    
    newState
