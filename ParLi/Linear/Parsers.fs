[<AutoOpen>]
module ParLi.Linear.Parsers


open ParLi.Parsers
open ParLi.Linear

/// Succeeds only if we have hit end of file
let inline eof< ^T, 'S when ^T: (member Length: int)> : MaybeParser<unit, ^T Input, 'S> =
    Parser.input
    |> Parser.map Input.``|EOF|_|``
    |> MaybeParser.ofParser

/// get: MaybeParser<'a, 'T Input, 'S> returns the current element of 'T
/// 'T must be one of: string, 'a list, 'a array
let inline get (input, state): 'a option * 'T Input * 'S =
    Input.tryGet input, input, state

/// pop: MaybeParser<'a, 'T Input, 'S> returns the current element of 'T and
/// advances the position by 1 if it succeeded
/// 'T must be one of: string, 'a list, 'a array
let inline pop (input, state): 'a option * 'T Input * 'S =
    let output = Input.tryGet input
    let nextInput = (if output.IsSome then Input.advance 1 else id) input
    output, nextInput, state
