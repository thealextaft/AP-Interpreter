namespace APMaths.Interpreter

module Lexer =
    open System
    open System.Globalization

    type Token =
        | INT of int
        | FLOAT of float
        | PLUS
        | MINUS
        | TIMES
        | DIVIDE
        | LPAREN
        | RPAREN
        | EOF

    type LexError = string

    let private isAsciiDigit (ch: char) = ch >= '0' && ch <= '9'

    let private readNumber (sourceText: string) (startIndex: int) : struct (Token * int) =
        let sourceLength = sourceText.Length
        let mutable currentIndex = startIndex
        let mutable integerDigitCount = 0
        let mutable fractionalDigitCount = 0
        let mutable hasDecimalPoint = false

        while currentIndex < sourceLength && isAsciiDigit sourceText[currentIndex] do
            integerDigitCount <- integerDigitCount + 1
            currentIndex <- currentIndex + 1

        if currentIndex < sourceLength && sourceText[currentIndex] = '.' then
            hasDecimalPoint <- true
            currentIndex <- currentIndex + 1
            while currentIndex < sourceLength && isAsciiDigit sourceText[currentIndex] do
                fractionalDigitCount <- fractionalDigitCount + 1
                currentIndex <- currentIndex + 1

        let lexeme = sourceText.Substring(startIndex, currentIndex - startIndex)
        if not hasDecimalPoint then
            if integerDigitCount = 0 then struct (EOF, startIndex) else
            if integerDigitCount > 3 then failwithf "Integer too long (max 3 digits): %s" lexeme
            else struct (INT (int lexeme), currentIndex)
        else
            if integerDigitCount = 0 || fractionalDigitCount = 0 then failwithf "incorrect float format: %s" lexeme
            if integerDigitCount > 2 || fractionalDigitCount > 2 then failwithf "Float must be up to 2 digits on each side of the decimal point: %s" lexeme
            let value = Double.Parse(lexeme, CultureInfo.InvariantCulture)
            struct (FLOAT value, currentIndex)

    let tokenize (inputText: string) : Result<Token list, LexError> =
        try
            let inputLength = inputText.Length
            let mutable currentIndex = 0
            let tokens = System.Collections.Generic.List<Token>()

            while currentIndex < inputLength do
                match inputText[currentIndex] with
                | ch when Char.IsWhiteSpace ch -> currentIndex <- currentIndex + 1
                | '+' -> tokens.Add(PLUS); currentIndex <- currentIndex + 1
                | '-' -> tokens.Add(MINUS); currentIndex <- currentIndex + 1
                | '*' | 'x' | 'X' -> tokens.Add(TIMES); currentIndex <- currentIndex + 1
                | '/' -> tokens.Add(DIVIDE); currentIndex <- currentIndex + 1
                | '(' -> tokens.Add(LPAREN); currentIndex <- currentIndex + 1
                | ')' -> tokens.Add(RPAREN); currentIndex <- currentIndex + 1
                | ch when isAsciiDigit ch ->
                    let struct (token, nextIndex) = readNumber inputText currentIndex
                    tokens.Add(token)
                    currentIndex <- nextIndex
                | '.' -> failwith "Float must include a leading digit (e.g., 0.5 not .5)."
                | unexpected -> failwithf "Unexpected character: '%c'" unexpected

            tokens.Add(EOF)
            Ok (List.ofSeq tokens)
        with ex -> Error ex.Message