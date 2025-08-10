namespace APMaths.Interpreter

module Ast =
    type Number =
        | Int of int
        | Float of float
        member number.AsFloat =
            match number with
            | Int integerValue -> float integerValue
            | Float floatValue -> floatValue

    type Expr =
        | Num of Number
        | UnaryMinus of Expr
        | Add of Expr * Expr
        | Subtract of Expr * Expr
        | Multiply of Expr * Expr
        | Divide of Expr * Expr
        | Parenthesised of Expr