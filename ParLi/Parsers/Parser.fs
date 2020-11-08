module ParLi.Parsers.Parser


/// run the parser on the input and state
let inline parseWith (parse: Parser<'a, 'T, 'S>) input state =
    parse (input, state)

/// Make a parser out of the function
let inline parser (func: _ -> ParseResult<_, _, _>): Parser<'a, 'input, 'state> =
    //  nothing actually happens because Parser<_,_,_> is a type abbreviation
    func

let ref (): (_ -> _) * Parser<'a, 'T, 'S> =
    let mutable parse = None
    (fun x -> parse <- Some x), parser (fun (input, state) -> parseWith parse.Value input state)

//  =========================
//       Basic Parsers
//  =========================

///// Always returns the result
//let inline retResult result: Parser<'a, 'T, 'S> =
//    parser (fun _ -> result)

///// Always returns the value
//let inline ret returnValue = retResult (ParseOk returnValue)

/// Always returns the value
let inline ret returnValue: Parser<'a, 'T, 'S> =
    parser (fun (t, s) -> ParseOk(returnValue, t, s))

/// A parser that always fails
let inline fail fatal: Parser<'a, 'T, 'S> =
    parser (fun (_, s) -> ParseError(fatal, s))

let inline failWeakly<'a, 'T, 'S> : Parser<'a, 'T, 'S> = fail false

let inline failFatally<'a, 'T, 'S> : Parser<'a, 'T, 'S> = fail true

/// equivalent to
///
///     function Ok x -> ret x | Error fatal -> fail
let inline retResult result: Parser<'a, 'T, 'S> =
    match result with
    | Ok x -> ret x
    | Error fatal -> fail fatal

//  =========================
//       The Bind Parser
//  =========================

let inline bind (binder: 'a -> Parser<'b, 'T, 'S>) p =
    parser (fun (input, state) ->
        match parseWith p input state with
        | ParseOk (output, nextInput, nextState) ->
            let nextParser = binder output
            parseWith nextParser nextInput nextState
        | ParseError (fatal, nextState) -> ParseError(fatal, nextState))

//  =========================
//       The Map Parser
//  =========================

/// Applies the function to the output 'a
let inline map (mapping: 'a -> 'b) p =
    parser (fun (input, state) ->
        match parseWith p input state with
        | ParseOk (output, newInput, newState) ->
            ParseOk(mapping output, newInput, newState)
        | ParseError (fatal, nextState) -> ParseError(fatal, nextState))

//  =========================
//       The Then Parser
//  =========================

/// Runs the first parser and threads the input and state to the next parsers
let inline andThen p1 p2: Parser<'a * 'b, 'T, 'S> =
    p1 |> bind (fun a -> p2 |> map (fun b -> a, b))

let inline andThenFst x y = andThen x y |> map fst

let inline andThenSnd x y = andThen x y |> map snd

//  =========================
//       Contextual Parsers
//  =========================

let inline input<'T, 'S> : Parser<'T, 'T, 'S> =
    parser (fun (input, state) -> ParseOk(input, input, state))

let inline state<'T, 'S> : Parser<'S, 'T, 'S> =
    parser (fun (input, state) -> ParseOk(state, input, state))

let inline updateInput mapping: Parser<unit, 'T, 'S> =
    parser (fun (input, state) -> ParseOk((), mapping input, state))

let inline updateState mapping: Parser<unit, 'T, 'S> =
    parser (fun (input, state) -> ParseOk((), input, mapping state))

//  =========================
//       The Or Parser
//  =========================

let inline orElse firstParser secondParser: Parser<'a, 'T, 'S> =
    parser (fun (input, state) ->
        match parseWith firstParser input state with
        | ParseError (false, nextState) ->
            parseWith secondParser input nextState
        | result -> result)

//  =========================
//       The Choose Parser
//  =========================

let inline chooseResult (choosing: 'a -> Result<_, _>) parser: Parser<'b, 'T, 'S> =
    bind (choosing >> retResult) parser

let inline choose (choosing: 'a -> 'b option) parser: Parser<'b, 'T, 'S> =
    chooseResult
        (choosing
         >> function
         | Some x -> Ok x
         | None -> Error false)
        parser

let inline where (predicate: 'a -> bool) parser: Parser<'a, 'T, 'S> =
    choose (fun x -> if predicate x then Some x else None) parser
