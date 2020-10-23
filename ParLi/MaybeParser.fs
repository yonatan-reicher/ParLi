module ParLi.MaybeParser

open ParLi


/// run the parser on the input and state
let inline parseWith (Parser parseFunction) input state: 'a * 'T * 'S =
    parseFunction (input, state)

let parser (func: 'input * 'state -> 'a option * 'input * 'state)
           : MaybeParser<'a, 'input, 'state> =
    func
    
let inline ofParser parser: MaybeParser<'a, 'T, 'S> = Parser.map Some parser
let inline toParser (MaybeParser parser): Parser<'a option, 'T, 'S> = parser

//  =========================
//       Basic Parsers
//  =========================

let inline ret returnValue: MaybeParser<'a, 'T, 'S> =
    Parser.ret (Some returnValue)

let inline fail<'a, 'T, 'S> : MaybeParser<'a, 'T, 'S> = Parser.ret None

//  =========================
//       The Bind Parser
//  =========================

let inline bind (binder: 'a -> MaybeParser<'b, 'T, 'S>) (MaybeParser parse) =
    Parser.bind (Option.map binder >> Option.defaultValue fail) parse

let inline (>>=) (p: MaybeParser<'a, 'T, 'S>) f: MaybeParser<'b, 'T, 'S> =
    bind f p

//  =========================
//       The Map Parser
//  =========================

let inline map (mapping: 'a -> 'b) (MaybeParser parse): MaybeParser<'b, 'T, 'S> =
    Parser.map (Option.map mapping) parse

let inline (|>>) (p: MaybeParser<'a, 'T, 'S>) (f: 'a -> 'b) = map f p

//  =========================
//       The Then Parser
//  =========================

let inline andThen (MaybeParser firstParse)
                   (MaybeParser secondParse)
                   : MaybeParser<'a * 'b, 'T, 'S> =
    firstParse >>= fun a -> secondParse |>> fun b -> a, b

let inline andThenFst x y = andThen x y |>> fst

let inline andThenSnd x y = andThen x y |>> snd

let (.>>.) = andThen

let (.>>) = andThenFst

let (>>.) = andThenSnd

let sequential x =
    let consParserResults (x: MaybeParser<'a list, 'T, 'S>)
                          (y: MaybeParser<'a, 'T, 'S>)
                          =
        map (fun (xn, x0) -> x0 :: xn) (andThen x y)

    //  Append the results in opposite order
    List.fold consParserResults (ret []) x
    //  And reverse it
    |>> List.rev

let inline tuple3 x y z: MaybeParser<'x * 'y * 'z, 'T, 'S> =
    x .>>. y .>>. z |>> fun ((x, y), z) -> x, y, z

let inline tuple4 x y z w: MaybeParser<'x * 'y * 'z * 'w, 'T, 'S> =
    x .>>. y .>>. z .>>. w
    |>> fun (((x, y), z), w) -> x, y, z, w

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

let inline (<|>) x y = orElse x y

let inline opt (MaybeParser parser) = ofParser parser

let inline choice x = List.reduce orElse x

/// Runs the parser many times and returns the input and state and a list of outputs
/// gathered up until the parser failed.
/// many p is a Parser and not a MaybeParser because it always succeeds
let inline many (MaybeParser p): Parser<'a list, 'T, 'S> =
    Parser.parser(fun (input, state) ->
        let mutable outputListReversed = []
        let mutable input = input
        let mutable state = state
        let mutable result = None

        while Option.isNone result do
            match parseWith p input state with
            | None, _, _ ->
                //  stop
                result <- Some (List.rev outputListReversed)
            | Some output, nextInput, nextState ->
                //  continue!
                outputListReversed <- output :: outputListReversed
                input <- nextInput
                state <- nextState
        Option.get result, input, state)
