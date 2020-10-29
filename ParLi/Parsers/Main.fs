[<AutoOpen>]
module ParLi.Parsers.Main


/// A parser is a function that takes a context ('input * 'state) and returns an
/// output 'a and an new context
/// 
/// 'input is the position, location or space that the parser will be parsing in
/// 'state is the context (errors, warnings, identifiers) that the parser will remember
/// 'a is the output (Parser<'a, 'input, 'state> is a monad over 'a)
type Parser<'a, 'input, 'state> = 'input * 'state -> 'a * 'input * 'state

/// (Parser parser) is the same as writing (parser: Parser<_,_,_>)
let (|Parser|) (parser: Parser<'a, 'input, 'state>) = parser


/// A maybe-parser is a function that takes a context ('input * 'state) and can return an
/// output 'a and an new context, or it can fail (and still update the state)
/// 
/// 'input is the position, location or space that the parser will be parsing in (sometimes will be 'T)
/// 'state is the context (errors, warnings, identifiers) that the parser will remember (sometimes will be 'S)
/// 'a is the output (Parser<'a, 'input, 'state> is a monad over 'a) (sometimes will be 'b)
type MaybeParser<'a, 'input, 'state> = Parser<'a option, 'input, 'state>

/// (MaybeParser parser) is the same as writing (parser: MaybeParser<_,_,_>)
let (|MaybeParser|) (parser: MaybeParser<'a, 'input, 'state>) = parser

