namespace APMaths.Interpreter

module Evaluator =
    open Ast

    type RuntimeError = string

    let rec private evaluateExpr (expr: Expr) : Result<float, RuntimeError> =
        match expr with
        | Num number -> Ok number.AsFloat
        | Parenthesised inner -> evaluateExpr inner
        | UnaryMinus inner -> evaluateExpr inner |> Result.map (fun value -> -value)
        | Add (left, right) ->
            match evaluateExpr left, evaluateExpr right with
            | Ok lv, Ok rv -> Ok (lv + rv)
            | Error e, _ | _, Error e -> Error e
        | Subtract (left, right) ->
            match evaluateExpr left, evaluateExpr right with
            | Ok lv, Ok rv -> Ok (lv - rv)
            | Error e, _ | _, Error e -> Error e
        | Multiply (left, right) ->
            match evaluateExpr left, evaluateExpr right with
            | Ok lv, Ok rv -> Ok (lv * rv)
            | Error e, _ | _, Error e -> Error e
        | Divide (left, right) ->
            match evaluateExpr left, evaluateExpr right with
            | Ok _, Ok 0.0 -> Error "Division by zero"
            | Ok lv, Ok rv -> Ok (lv / rv)
            | Error e, _ | _, Error e -> Error e

    let Evaluate (expr: Expr) = evaluateExpr expr