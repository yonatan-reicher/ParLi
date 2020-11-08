[<AutoOpen>]
module ParLi.Parsers.Main


/// ParseResult is a type returned by parsing with a parser on an input
/// A parse result can be successful and have an output or fail weakly (recoverable) or fail fatally (unrecoverable)
[<Struct>]
type ParseResult<'a, 'input, 'state> = 
    | ParseOk of output: 'a * 'input * okState: 'state
    | ParseError of fatal: bool * errorState: 'state


/// A parser is a function that takes a context ('input * 'state) and returns an
/// output 'a and an new context
/// 
/// 'input is the position, location or space that the parser will be parsing in
/// 'state is the context (errors, warnings, identifiers) that the parser will remember
/// 'a is the output (Parser<'a, 'input, 'state> is a monad over 'a)
type Parser<'a, 'input, 'state> = 'input * 'state -> ParseResult<'a, 'input, 'state>
