module ParLi.Parsers.MaybeParser


/// run the parser on the input and state
let inline parseWith (MaybeParser parseFunction): _ -> _ -> 'a option * 'T * 'S =
    Parser.parseWith parseFunction


// make a maybe-parser out of a function
let inline maybeParser func: MaybeParser<'a, 'input, 'state> =
    MaybeParser(Parser.parser func)

let inline ofParser parser: MaybeParser<'a, 'T, 'S> = MaybeParser parser
let inline toParser (MaybeParser parser): Parser<'a option, 'T, 'S> = parser

let ref (): (_ -> _) * MaybeParser<'a, 'T, 'S> = 
    let mutable parse = None
    (fun x -> parse <- Some x), maybeParser (fun (input, state) -> parseWith parse.Value input state)

//  =========================
//       Basic Parsers
//  =========================

/// Always returns the value
let inline ret returnValue: MaybeParser<'a, 'T, 'S> =
    Parser.ret (Some returnValue) |> ofParser

/// Always fails
let inline fail<'a, 'T, 'S> : MaybeParser<'a, 'T, 'S> =
    Parser.ret None |> ofParser

let inline some parser: MaybeParser<'a, 'T, 'S> =
    Parser.map Some parser |> ofParser

let inline ofOption value: MaybeParser<'a, 'T, 'S> =
    Parser.ret value |> ofParser

//  =========================
//       The Bind Parser
//  =========================

let inline bind (binder: 'a -> MaybeParser<'b, 'T, 'S>) (MaybeParser parser) =
    let parseBinder: 'a option -> Parser<'b option, 'T, 'S> =
        Option.map (binder >> toParser)
        >> Option.defaultValue (Parser.ret None)

    Parser.bind parseBinder parser |> ofParser

//  =========================
//       The Map Parser
//  =========================

let inline map (mapping: 'a -> 'b) (MaybeParser parse): MaybeParser<'b, 'T, 'S> =
    Parser.map (Option.map mapping) parse |> ofParser

//  =========================
//       The Then Parser
//  =========================

let inline andThen firstParser secondParser: MaybeParser<'a * 'b, 'T, 'S> =
    firstParser
    |> bind (fun a -> secondParser |> map (fun b -> a, b))

let inline andThenFst x y = andThen x y |> map fst

let inline andThenSnd x y = andThen x y |> map snd

//  =========================
//       The Or Parser
//  =========================

let inline orElse firstParser secondParser: MaybeParser<'a, 'T, 'S> =
    maybeParser (fun (input, state) ->
        match parseWith firstParser input state with
        | None, _, newState -> parseWith secondParser input newState
        | result -> result)

let inline defaultWith defaultParser parser: Parser<'a, 'T, 'S> =
    Parser.parser (fun (input, state) ->
        match parseWith parser input state with
        | Some result, input, state -> result, input, state
        | None, _, newState -> Parser.parseWith defaultParser input newState)

//let inline opt (MaybeParser parser) = ofParser parser

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
            | None, _, _ ->
                //  stop
                result <- Some(List.rev outputListReversed)
            | Some output, nextInput, nextState ->
                //  continue!
                outputListReversed <- output :: outputListReversed
                input <- nextInput
                state <- nextState

        Option.get result, input, state)

//  =========================
//       The Choose Parser
//  =========================

let inline choose (choosing: 'a -> 'b option) parser: MaybeParser<'b, 'T, 'S> =
    bind (choosing >> ofOption) parser

let inline onlyChoose (choosing: 'a -> 'b option) parser: MaybeParser<'b, 'T, 'S> =
    Parser.bind (choosing >> Parser.ret) parser |> ofParser

let inline where (predicate: 'a -> bool) parser: MaybeParser<'a, 'T, 'S> =
    choose (fun x -> if predicate x then Some x else None) parser

let inline onlyWhere (predicate: 'a -> bool) parser: MaybeParser<'a, 'T, 'S> =
    onlyChoose (fun x -> if predicate x then Some x else None) parser
