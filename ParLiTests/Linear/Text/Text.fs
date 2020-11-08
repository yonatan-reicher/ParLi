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

let failWith (message: 'message) =
    Parser.input
    >>= fun input ->
            Parser.updateState ((@) [ message, Input.position input ])
            >>. Parser.failFatally

let identifier =
    charsTake (Char.IsWhiteSpace >> not)
    |> Parser.where (String.IsNullOrEmpty >> not)

let number =
    charsTake Char.IsDigit
    |> Parser.where (String.IsNullOrEmpty >> not)
    |>> int

let expri, (expr: Parser<AST, Input, _>) = Parser.ref ()

let atom =
    choice [ number |>> Const
             identifier |>> Var ]

let term' =
    atom
    .>>. opt
             (spacesln
              >>. stringSkip "*"
              >>. spacesln
              >>. (atom <|> failWith "Expected an atom"))
    |>> function
    | a, Some b -> Mul(a, b)
    | a, None -> a

let term =
    term'
    .>>. opt
             (spacesln
              >>. stringSkip "+"
              >>. spacesln
              >>. (term' <|> failWith "Expected an small term"))
    |>> function
    | a, Some b -> Add(a, b)
    | a, None -> a

let ``let`` =
    (stringSkip "let" >>. spaces >>. identifier
     .>> spaces
     .>> (stringSkip "=" <|> failWith "Expected a = symbol")
     .>> spaces
     .>>. (expr <|> failWith "Expected an expression")
     .>> spaces
     .>> (stringSkip "in" <|> failWith "Expected an in keyword")
     .>> spacesln
     .>>. (expr <|> failWith "Expected an expression"))
    |>> fun ((a, b), c) -> Let(a, b, c)

do expri (choice [ ``let``; term ])

[<Fact>]
let ``My test`` () =
    let text = @"let x = 2 + 5 * 3 in
x * x"

    parseWith expr (Input.ofValue text) []
    |> function
    | ParseError (fatal, state) -> Assert.True(false, "result was not ParseOk")
    | ParseOk (output, input, state) ->
        Assert.Equal<list<_>>([], state)

        Assert.Equal
            (Let
                ("x", Add(Const 2, Mul(Const 5, Const 3)), Mul(Var "x", Var "x")),
             output)

        Assert.True(Input.eof input)

[<Fact>]
let ``fails correctly`` () =
    let text = @"let x = 2 + 6 * 9
x * x"

    let text = parseWith expr (Input.ofValue text) []

    Assert.Equal(ParseError(true, ["Expected an in keyword", Position.position 17]), text)
