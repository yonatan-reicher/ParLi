module ParLi.Parsers.MaybeParser


/// run the parser on the input and state
let inline parseWith (Parser parseFunction) input state: 'a * 'T * 'S =
    parseFunction (input, state)


//  Nothing actually happens in these next functions because the
//  parser is the function this is purely for typing reasons

// make a parser out of a function
let inline parser (func: 'input * 'state -> 'a option * 'input * 'state)
                  : MaybeParser<'a, 'input, 'state> =
    func

let inline ofParser (Parser parser): MaybeParser<'a, 'T, 'S> = parser
let inline toParser (MaybeParser parser): Parser<'a option, 'T, 'S> = parser

//  =========================
//       Basic Parsers
//  =========================

/// Always returns the value
let inline ret returnValue: MaybeParser<'a, 'T, 'S> =
    Parser.ret (Some returnValue)

/// Always fails
let inline fail<'a, 'T, 'S> : MaybeParser<'a, 'T, 'S> = Parser.ret None

let inline some parser: MaybeParser<'a, 'T, 'S> = Parser.map Some parser |> ofParser

//  =========================
//       The Bind Parser
//  =========================

let inline bind (binder: 'a -> MaybeParser<'b, 'T, 'S>) (MaybeParser parse) =
    Parser.bind (Option.map binder >> Option.defaultValue fail) parse

//  =========================
//       The Map Parser
//  =========================

let inline map (mapping: 'a -> 'b) (MaybeParser parse): MaybeParser<'b, 'T, 'S> =
    Parser.map (Option.map mapping) parse

//  =========================
//       The Then Parser
//  =========================

let inline andThen (MaybeParser firstParse)
                   (MaybeParser secondParse)
                   : MaybeParser<'a * 'b, 'T, 'S> =
    firstParse
    |> bind (fun a -> secondParse |> map (fun b -> a, b))

let inline andThenFst x y = andThen x y |> map fst

let inline andThenSnd x y = andThen x y |> map snd

//  =========================
//       The Or Parser
//  =========================

let inline orElse (MaybeParser firstParse)
                  (MaybeParser secondParse)
                  : MaybeParser<'a, 'T, 'S> =
    fun (input, state) ->
        match firstParse (input, state) with
        | None, _, newState -> secondParse (input, newState)
        | result -> result

let inline defaultWith (MaybeParser parse)
                       (Parser defaultParse)
                       : Parser<'a, 'T, 'S> =
    Parser.parser (fun (input, state) ->
        match parseWith parse input state with
        | Some result, input, state -> result, input, state
        | None, _, newState -> Parser.parseWith defaultParse input newState)

//let inline opt (MaybeParser parser) = ofParser parser

/// Runs the parser many times and returns the input and state and a list of outputs
/// gathered up until the parser failed.
/// many p is a Parser and not a MaybeParser because it always succeeds
let inline many (MaybeParser p): Parser<'a list, 'T, 'S> =
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
    bind (choosing >> Parser.ret) parser

let inline onlyChoose (choosing: 'a -> 'b option) parser: MaybeParser<'b, 'T, 'S> =
    Parser.bind (choosing >> Parser.ret) parser

let inline where (predicate: 'a -> bool) parser: MaybeParser<'a, 'T, 'S> =
    choose (fun x -> if predicate x then Some x else None) parser

let inline onlyWhere (predicate: 'a -> bool) parser: MaybeParser<'a, 'T, 'S> =
    onlyChoose (fun x -> if predicate x then Some x else None) parser
