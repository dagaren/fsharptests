open System
open Domain
open Commands
open Parsing
open IO

let readCommandsSeq = seq {
    while true  do
        yield (readInput ())
}

let filterValidCommand parseResult = 
    match parseResult with
     | Ok command -> Some command
     | Error _ -> sendOutput "Invalid command"; None

[<EntryPoint>]
let main argv =

    readCommandsSeq
     |> Seq.map parseCommand
     |> Seq.choose filterValidCommand
     |> Seq.takeWhile (not << isExitCommand)
     |> Seq.fold handleCommand Commands.initialState
     |> ignore

    printfn "-- Normal termination of the program --"
    0 // return an integer exit code