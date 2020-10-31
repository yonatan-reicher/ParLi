module Linear.Text.Tests

open System
open Xunit

open ParLi.Parsers
open ParLi.Linear
open ParLi.Text


let text = @"let x = 2 + 5 * 3 in
x * x;"

let identifier: MaybeParser<_, _, unit> = charsTake (Char.IsWhiteSpace >> not) |> MaybeParser.onlyWhere (String.IsNullOrEmpty >> not)

type AST =
    | Const of int
    | Var of string
    | Add of AST * AST
    | Mul of AST * AST
    | Let of string * AST * AST

let expri, (expr: MaybeParser<AST, Input, unit>) = Parser.ref ()

let ``let``: MaybeParser<AST, Input, unit> =
    let beforeEq: MaybeParser<string, Input, unit> = stringSkip "let" >>? surroundedSpaces identifier
    let beforeIn = char '=' >>. surroundedSpaces expr
    let afterIn = string "in" >>. spacesln >>. expr
    beforeEq .>>. beforeIn .>>. afterIn |>> failwithf ""

do expri (choice [ ``let`` ])

[<Fact>]
let ``My test`` () =
    let output, input, state = Parser.parseWith expr (Input.ofValue text) ()

    Assert.Equal
        (Some
         <| Let("x", Add(Const 2, Mul(Const 5, Const 3)), Mul(Var "x", Var "x")),
         output)

    Assert.True(Input.eof input)
    Assert.Equal(state, ())
