module ParLi.Parsers.Parser


/// run the parser on the input and state
let inline parseWith (Parser parseFunction) input state: 'a * 'T * 'S =
    parseFunction (input, state)

/// Make a parser out of the function
let parser func: Parser<'a, 'input, 'state> =
    Parser func

let ref (): (_ -> _) * Parser<'a, 'T, 'S> = 
    let mutable parse = None
    (fun x -> parse <- Some x), parser (fun (input, state) -> parseWith parse.Value input state)

//  =========================
//       Basic Parsers
//  =========================

/// Always returns the value
let inline ret returnValue: Parser<'a, 'T, 'S> =
    parser (fun (t, s) -> returnValue, t, s)

//  =========================
//       The Map Parser
//  =========================

/// Applies the function to the output 'a
let inline map (mapping: 'a -> 'b) (Parser parse: Parser<'a, 'T, 'S>) =
    parser (fun (input, state) ->
        let output, newInput, newState = parse (input, state)
        mapping output, newInput, newState)

//  =========================
//       The Then Parser
//  =========================

/// Runs the first parser and threads the input and state to the next parsers
let inline andThen (Parser firstParse)
                   (Parser secondParse)
                   : Parser<'a * 'b, 'T, 'S> =
    parser (fun (input, state) ->
        let output1, newInput, newState = firstParse (input, state)
        let output2, finalInput, finalState = secondParse (newInput, newState)
        (output1, output2), finalInput, finalState)

let inline andThenFst x y = andThen x y |> map fst

let inline andThenSnd x y = andThen x y |> map snd

//let sequential x =
//    let consParserResults (x: Parser<'a list, 'T, 'S>) (y: Parser<'a, 'T, 'S>) =
//        map (fun (xn, x0) -> x0 :: xn) (andThen x y)

//    //  Append the results in opposite order
//    List.fold consParserResults (ret []) x
//    //  And reverse it
//    |>> List.rev

//let inline tuple3 x y z: Parser<'x * 'y * 'z, 'T, 'S> =
//    x .>>. y .>>. z |>> fun ((x, y), z) -> x, y, z

//let inline tuple4 x y z w: Parser<'x * 'y * 'z * 'w, 'T, 'S> =
//    x .>>. y .>>. z .>>. w
//    |>> fun (((x, y), z), w) -> x, y, z, w

//  =========================
//       The Bind Parser
//  =========================

let inline bind (binder: 'a -> Parser<'b, 'T, 'S>) (Parser parse) =
    parser (fun input ->
        let output, newInput, newState = parse input
        let (Parser nextParse) = binder output
        nextParse (newInput, newState))

//  =========================
//       Contextual Parsers
//  =========================

let inline input<'T, 'S> : Parser<'T, 'T, 'S> =
    parser (fun (input, state) -> input, input, state)

let inline state<'T, 'S> : Parser<'S, 'T, 'S> =
    parser (fun (input, state) -> state, input, state)

let inline updateInput mapping: Parser<unit, 'T, 'S> =
    parser (fun (input, state) -> (), mapping input, state)

let inline updateState mapping: Parser<unit, 'T, 'S> =
    parser (fun (input, state) -> (), input, mapping state)
