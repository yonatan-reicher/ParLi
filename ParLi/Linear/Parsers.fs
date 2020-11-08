[<AutoOpen>]
module ParLi.Linear.Parsers


open ParLi.Parsers
open ParLi.Linear

/// Succeeds only if we have hit end of file
let inline eof< ^T, 'S when ^T: (member Length: int)> : Parser<unit, ^T Input, 'S> =
    Parser.input
    |> Parser.choose Input.``|EOF|_|``

/// get: MaybeParser<'a, 'T Input, 'S> returns the current element of 'T
/// 'T must be one of: string, 'a list, 'a array
let inline get (): Parser<'a, 'T Input, 'S> =
    Parser.input |> Parser.choose Input.tryGet

/// pop: MaybeParser<'a, 'T Input, 'S> returns the current element of 'T and
/// advances the position by 1 if it succeeded
/// 'T must be one of: string, 'a list, 'a array
let inline pop (): Parser<'a, 'T Input, 'S> =
    get () .>> Parser.updateInput (Input.advance 1)
