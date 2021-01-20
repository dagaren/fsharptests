open System
open FParsec

type Date = 
 | Date of year:int * month:int * day:int
 | Yesterday
 | Today

type AddWordCommand = AddWordCommand of word:string
type RemoveWordCommand = RemoveWordCommand of word:string
type SetCurrentDateCommand = SetCurrentDateCommand of Date

type Command =
 | SetCurrentDate of SetCurrentDateCommand
 | AddWord of AddWordCommand
 | RemoveWord of RemoveWordCommand
 | Exit

type AppState = {
    selectedDate: DateTime option
    words: string list
}

let sendOutput commandText =
    printfn ">: %s" commandText

let pword = many1Satisfy isAsciiLetter

let pDate = 
    (stringCIReturn "today" Today) <|> (stringCIReturn "yesterday" Yesterday)

let pSetCurrentDateCommand = 
    skipStringCI "SetCurrentDate" >>. spaces >>. pDate |>> SetCurrentDateCommand |>> SetCurrentDate .>> eof

let pcommand = 
    (stringCIReturn "exit" Exit) <|>
    (skipStringCI "AddWord" >>. spaces >>. pword .>> eof |>> AddWordCommand |>> AddWord) <|>
    (skipStringCI "RemoveWord" >>. spaces >>. pword .>> eof |>> RemoveWordCommand |>> RemoveWord) <|>
    pSetCurrentDateCommand

let parseCommand text =
    let r = run pcommand text
    match r with 
     | Success (result, _, _) -> Result.Ok result
     | _ -> Result.Error "Invalid command"

let readCommandsSeq = seq {
    while true  do
        printf "<: "
        yield Console.ReadLine()
}

let filterValidCommand parseResult = 
    match parseResult with
     | Result.Ok command -> Some command
     | Result.Error _ -> sendOutput "Invalid command"; None

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
    let newState = {state with words = newWords }
    sendOutput (sprintf "%A" newState.words)
    newState

let executeSetCurrentDateCommand state (SetCurrentDateCommand date) =
    sendOutput (sprintf "Set current date %A" date)
    state

let handleCommand state command = 
    let newState = match command with
     | AddWord command -> executeAddWordCommand state command
     | RemoveWord command -> executeRemoveWordCommand state command
     | SetCurrentDate command -> executeSetCurrentDateCommand state command
     | Exit -> sendOutput "exit"; state
    
    newState

[<EntryPoint>]
let main argv =
    let initialState = {
        words = []
        selectedDate = None
    }

    readCommandsSeq
     |> Seq.map parseCommand
     |> Seq.choose filterValidCommand
     |> Seq.takeWhile isNotExitCommand
     |> Seq.fold handleCommand initialState
     |> ignore

    printfn "-- Normal termination of the program --"
    0 // return an integer exit code