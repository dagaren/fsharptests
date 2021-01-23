module Parsing

open FParsec
open Domain

let pWord = many1Satisfy isAsciiLetter

let pDate = 
    (stringCIReturn "today" Today) <|> 
    (stringCIReturn "yesterday" Yesterday) <|>
    (pipe3 (pint32 .>> skipString "/") (pint32 .>> skipString "/") (pint32) (fun x y z -> Date (x, y, z)))


let pSetCurrentDateCommand = 
    skipStringCI "SetCurrentDate" >>. spaces >>. pDate |>> SetCurrentDateCommand |>> SetCurrentDate .>> eof

let pGetCurrentDateCommand =
    stringCIReturn "GetCurrentDate" GetCurrentDate

let pcommand = 
    (stringCIReturn "exit" Exit) <|>
    (skipStringCI "AddWord" >>. spaces >>. pWord .>> eof |>> AddWordCommand |>> AddWord) <|>
    (skipStringCI "RemoveWord" >>. spaces >>. pWord .>> eof |>> RemoveWordCommand |>> RemoveWord) <|>
    pSetCurrentDateCommand <|>
    pGetCurrentDateCommand

let parseCommand text =
    let r = run pcommand text
    match r with 
     | Success (result, _, _) -> Result.Ok result
     | _ -> Result.Error "Invalid command"
