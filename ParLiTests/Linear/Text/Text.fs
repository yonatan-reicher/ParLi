module Linear.Text.Tests

open System
open Xunit

open ParLi.Parsers
open ParLi.Linear
open ParLi.Text


type AST =
    | Const of int
    | Var of string
    | Add of AST * AST
    | Mul of AST * AST
    | Let of string * AST * AST

let text = @"let x = 2 + 5 * 3 in
x * x"

let failWith message = MaybeParser.some Parser.input >>= fun input -> Parser.updateState ((@) [message, Input.position input]) >>. MaybeParser.fail

let expect string =
    stringSkip string
    <|> failWith ("Expected " + string)

let identifier: MaybeParser<_, _, _> =
    choice [ charsTake (Char.IsWhiteSpace >> not)
             |> MaybeParser.onlyWhere (String.IsNullOrEmpty >> not)

             failWith "expected identifier" ]

let number: MaybeParser<_, _, _> =
    choice [ charsTake Char.IsDigit
             |> MaybeParser.onlyWhere (String.IsNullOrEmpty >> not)
             |>> int

             failWith "expected number" ]

let expri, (expr: MaybeParser<AST, Input, _>) = MaybeParser.ref ()

let atom = 
    choice [
        discardState number |>> Const
        discardState identifier |>> Var
    ] <|> failWith "expected atom"

let term': MaybeParser<AST, _, _> =
    atom .>>. onlyIf (spacesln >>. stringSkip "*" .>> spacesln) atom
    |>> function a, Some b -> Mul (a, b) | a, None -> a

let term: MaybeParser<AST, _, _> = 
    term' .>>. onlyIf (spacesln >>. stringSkip "+" .>> spacesln) term'
    |>> function a, Some b -> Add (a, b) | a, None -> a

let ``let``: MaybeParser<AST, Input, _> =
    (expect "let" >>. spaces >>. identifier
     .>> spaces
     .>> expect "="
     .>> spaces
     .>>. expr
     .>> spaces
     .>> expect "in"
     .>> spacesln
     .>>. expr)
    |>> fun ((a, b), c) -> Let(a, b, c)

do expri (choice [ ``let``; term ])

[<Fact>]
let ``My test`` () =
    let output, input, state =
        MaybeParser.parseWith expr (Input.ofValue text) []
            
    Assert.Equal<list<_>>([], state)
    Assert.Equal
        (Some
         <| Let("x", Add(Const 2, Mul(Const 5, Const 3)), Mul(Var "x", Var "x")),
         output)
    Assert.True(Input.eof input)
