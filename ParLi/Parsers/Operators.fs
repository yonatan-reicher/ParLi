[<AutoOpen>]
module ParLi.Parsers.Operators

open ParLi.Parsers

[<AutoOpen>]
module BasicOperators =
    /// a parser that always returns this value
    let inline ret (value: 'a): Parser<'a, 'T, 'S> = Parser.ret value

    /// return the output of applying the parser on the input and state
    let inline parseWith (parser: Parser<'a, 'T, 'S>) (input: 'T) (state: 'S) = 
        Parser.parseWith parser input state

    /// map the output of a parser
    let inline (|>>) (x: Parser<'a, 'T, 'S>) (f: 'a -> 'b): Parser<'b, 'T, 'S> = Parser.map f x

    /// bind the output of a parser
    let inline (>>=) p (binder: 'a -> Parser<'b, 'T, 'S>): Parser<'b, 'T, 'S> = Parser.bind binder p

    /// return the output of parser1 or parser2
    let inline (<|>) x y: Parser<'a, 'T, 'S> = Parser.orElse x y
    
    /// return the output of parsing with parser1 and then parser2
    let inline (.>>.) x y: Parser<'a * 'b, 'T, 'S> = Parser.andThen x y
    let inline (.>>) x y: Parser<'a, 'T, 'S> = Parser.andThenFst x y
    let inline (>>.) x y: Parser<'b, 'T, 'S> = Parser.andThenSnd x y


[<AutoOpen>]
module CompoundOperators = 

    /// Apply a series of parsers one after the other
    let inline sequential parsers: Parser<'a list, 'T, 'S> = 
        List.map (Parser.map List.singleton) parsers
        |> List.reduce (fun x y -> x .>>. y |>> ((<||) (@)))

    /// Apply each parser until one succeeds
    let inline choice parsers: Parser<'a, 'T, 'S> =
        List.reduce (<|>) parsers

    /// After applying the parser `parser`, return the state to its original value
    let inline discardState parser: Parser<'a,'T,'S> =
        Parser.state >>= fun state -> parser .>> Parser.updateState (fun _ -> state)

    /// Runs the parser many times and returns the input and state and a list of outputs
    /// gathered up until the parser failed.
    /// many p is a Parser and not a MaybeParser because it always succeeds
    let inline many p: Parser<'a list, 'T, 'S> =
        Parser.parser (fun (input, state) ->
            let mutable outputListReversed = []
            let mutable input = input
            let mutable state = state
            let mutable result = None
    
            while Option.isNone result do
                match parseWith p input state with
                | ParseError (true, nextState) ->
                    //  fail!
                    state <- nextState
                    result <- Some(ParseError(true, state))
                | ParseError (false, nextState) ->
                    //  stop
                    state <- nextState
    
                    result <-
                        Some(ParseOk(List.rev outputListReversed, input, state))
                | ParseOk (output, nextInput, nextState) ->
                    //  continue!
                    outputListReversed <- output :: outputListReversed
                    input <- nextInput
                    state <- nextState
    
            Option.get result)

    let inline opt (p: Parser<'a, 'T, 'S>) = 
        p |>> Some <|> ret None

    let inline tuple3 p1 p2 p3 = 
        p1 .>>. p2 .>>. p3 |>> fun ((a, b), c) -> a, b, c

    let inline tuple4 p1 p2 p3 p4 = 
        p1 .>>. p2 .>>. p3 .>>. p4 |>> fun (((a, b), c), d) -> a, b, c, d
