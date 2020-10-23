module Linear.Text.Tests

open System
open Xunit

open ParLi
open ParLi.Linear
open ParLi.Linear.Text

let text = String.replicate 20_000 "h" |> Input.ofValue
let p1 = string "hhh"

[<Fact>]
let ``My test`` () =
    let output, input, state = Parser.parseWith p text ()
    Assert.Equal(Some "hhh", output)
    Assert.Equal(Input.advance 3 text, input)
    Assert.Equal(state, ())
    
    for i in 0..10000 do 
        let output, input, state = Parser.parseWith p text ()
