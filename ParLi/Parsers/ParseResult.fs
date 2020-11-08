module ParLi.Parsers.ParseResult

open ParLi.Parsers


let ok: _ -> ParseResult<'a, 'T, 'S> = ParseOk
let fail: _ -> ParseResult<'a, 'T, 'S> = ParseError

let toResult: ParseResult<'a, 'T, 'S> -> _ = function 
    | ParseOk (a, b, c) -> Ok (a, b, c)
    | ParseError (a, b) -> Error (a, b)

let ofResult: _ -> ParseResult<'a, 'T, 'S> = function
    | Ok (a, b, c) -> ok (a, b, c)
    | Error (a, b) -> fail (a, b)
