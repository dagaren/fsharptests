module IO

open System

let sendOutput commandText =
    printfn ">: %s" commandText

let readInput () = 
    printf "<: "
    Console.ReadLine()