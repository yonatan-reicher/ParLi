module Linear.Text.Tests

open System
open Xunit

open ParLi.Parser
open ParLi.Linear
open ParLi.Linear.Text

open MaybeParser

let text = @"let x = 2 + 5 * 3 in
x * x;"

let identifier: Parser<_,_,unit> = charsTake (Char.IsWhiteSpace >> not)
let whitespace = spaces <|> newline

let ``let`` = stringSkip "let" >>. 

let p1 =
    choice [
        stringSkip "let" >>. spaces >>. 
    ]

[<Fact>]
let ``My test`` () =
    let output, input, state = Parser.parseWith p1 text ()
    
    Assert.Equal(Some "hhh", output)
    Assert.Equal(Input.advance 3 text, input)
    Assert.Equal(state, ())
    
