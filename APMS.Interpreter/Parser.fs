namespace APMaths.Interpreter

module Parser =
    open Lexer
    open Ast

    type ParseError = string

    type private ParserState = { remaining: Token list }

    let private peek (state: ParserState) =
        match state.remaining with
        | token :: _ -> token
        | [] -> EOF

    let private advance (state: ParserState) =
        match state.remaining with
        | _ :: tail -> { remaining = tail }
        | [] -> state

    let private expect (expected: Token) (state: ParserState) : Result<ParserState, ParseError> =
        let actual = peek state
        if actual = expected then Ok (advance state)
        else Error (sprintf "Expected %A but found %A" expected actual)

    let rec private parseExpression (state: ParserState) : Result<Expr * ParserState, ParseError> =
        parseAdditionOrSubtraction state

    and private parseAdditionOrSubtraction (state: ParserState) : Result<Expr * ParserState, ParseError> =
        match parseMultiplicationOrDivision state with
        | Error e -> Error e
        | Ok (firstTerm, afterFirst) ->
            let rec loop (accumulatedExpr: Expr) (currentState: ParserState) =
                match peek currentState with
                | PLUS ->
                    let afterPlus = advance currentState
                    match parseMultiplicationOrDivision afterPlus with
                    | Ok (rhs, nextState) -> loop (Add (accumulatedExpr, rhs)) nextState
                    | Error e -> Error e
                | MINUS ->
                    let afterMinus = advance currentState
                    match parseMultiplicationOrDivision afterMinus with
                    | Ok (rhs, nextState) -> loop (Subtract (accumulatedExpr, rhs)) nextState
                    | Error e -> Error e
                | _ -> Ok (accumulatedExpr, currentState)
            loop firstTerm afterFirst

    and private parseMultiplicationOrDivision (state: ParserState) : Result<Expr * ParserState, ParseError> =
        match parseUnary state with
        | Error e -> Error e
        | Ok (firstFactor, afterFirst) ->
            let rec loop (accumulatedExpr: Expr) (currentState: ParserState) =
                match peek currentState with
                | TIMES ->
                    let afterTimes = advance currentState
                    match parseUnary afterTimes with
                    | Ok (rhs, nextState) -> loop (Multiply (accumulatedExpr, rhs)) nextState
                    | Error e -> Error e
                | DIVIDE ->
                    let afterDivide = advance currentState
                    match parseUnary afterDivide with
                    | Ok (rhs, nextState) -> loop (Divide (accumulatedExpr, rhs)) nextState
                    | Error e -> Error e
                | _ -> Ok (accumulatedExpr, currentState)
            loop firstFactor afterFirst

    and private parseUnary (state: ParserState) : Result<Expr * ParserState, ParseError> =
        match peek state with
        | MINUS ->
            let afterMinus = advance state
            match parseUnary afterMinus with
            | Ok (expr, nextState) -> Ok (UnaryMinus expr, nextState)
            | Error e -> Error e
        | _ -> parsePrimary state



        /// parses a primary expression, which can be a number or a parenthesised expression.
    and private parsePrimary (state: ParserState) : Result<Expr * ParserState, ParseError> =
        match peek state with
        | INT i -> Ok (Num (Number.Int i), advance state)
        | FLOAT f -> Ok (Num (Number.Float f), advance state)
        | LPAREN ->
            let afterLParen = advance state
            match parseExpression afterLParen with
            | Ok (innerExpr, afterInner) ->
                match expect RPAREN afterInner with
                | Ok afterRParen -> Ok (Parenthesised innerExpr, afterRParen)
                | Error e -> Error e
            | Error e -> Error e
        | unexpected -> Error (sprintf "Unexpected token: %A" unexpected)

    let parse (tokens: Token list) : Result<Ast.Expr, ParseError> =
        let initial = { remaining = tokens }
        match parseExpression initial with
        | Ok (expr, stateAfterExpr) ->
            match peek stateAfterExpr with
            | EOF -> Ok expr
            | trailing -> Error (sprintf "Unexpected trailing token: %A" trailing)
        | Error e -> Error e