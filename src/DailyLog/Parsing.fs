module Parsing

open FParsec
open Commands

let pDate = 
    (stringCIReturn "today" Today) <|> 
    (stringCIReturn "yesterday" Yesterday) <|>
    (pipe3 (pint32 .>> skipString "/") (pint32 .>> skipString "/") (pint32) (fun x y z -> Date { year = x; month = y; day = z }))

let pSetCurrentDateCommand = 
    skipStringCI "date" >>. spaces >>. pDate |>> SetCurrentDateCommand |>> SetCurrentDate .>> eof

let pGetCurrentDateCommand =
    stringCIReturn "date" GetCurrentDate .>> eof

let pExit = 
    stringCIReturn "exit" Exit .>> eof

let pShowLogs = 
    stringCIReturn "logs" ShowLogs .>> eof

let pString: Parser<string, unit> = many1CharsTill anyChar eof

let pAddLog =
    skipStringCI "log" >>. spaces >>. pString |>> AddLogCommand |>> AddLog .>> eof 

let pcommand = 
    choice [
      attempt pExit
      attempt pGetCurrentDateCommand
      attempt pSetCurrentDateCommand
      attempt pShowLogs
      attempt pAddLog
    ]

let parseCommand text =
    let r = run pcommand text
    match r with 
     | Success (result, _, _) -> Result.Ok result
     | _ -> Result.Error "Invalid command"
