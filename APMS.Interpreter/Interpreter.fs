namespace APMaths.Interpreter



module Interpreter =
    open Lexer
    open Parser
    open Evaluator

    type EvalResult =
        | Ok of float
        | Error of string

    let Evaluate (sourceText: string) : EvalResult =
        match Lexer.tokenize sourceText with
        | Result.Error lexErr ->
            EvalResult.Error (sprintf "Lex error: %A" lexErr)

        | Result.Ok tokens ->
            match Parser.parse tokens with
            | Result.Error parseErr ->
                EvalResult.Error (sprintf "Parse error: %A" parseErr)

            | Result.Ok ast ->
                match Evaluator.Evaluate ast with
                | Result.Ok value ->
                    EvalResult.Ok value
                | Result.Error runtimeErr ->
                    EvalResult.Error (sprintf "Runtime error: %A" runtimeErr)
