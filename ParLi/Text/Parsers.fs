[<AutoOpen>]
module ParLi.Text.Parsers

open System
open System.Text.RegularExpressions

open ParLi.Parsers
open ParLi.Linear
open ParLi.Parsers

let inline char (ch: char): Parser<char, Input, 'S> =
    pop () |> Parser.where ((=) ch)

let inline string (str: string): Parser<string, Input, 'S> =
    Parser.parser (fun (input, state) ->
        let (Input (inputString, Position i)) = input
        let len = str.Length

        if inputString.[i..(i + len - 1)] = str then
            ParseOk (str, Input.advance len input, state)
        else
            ParseError (false, state))

let regex pattern: Parser<string, Input, 'S> = 
    let regexObj = new Regex (pattern, RegexOptions.Compiled)

    Parser.parser (fun (input, state) ->
        let matchObj = regexObj.Match(Input.rest input)
        if matchObj.Success then
            let substr = matchObj.Value
            ParseResult.ok (substr, Input.advance substr.Length input, state)
        else 
            ParseResult.fail (false, state))

let inline stringReturn str value: Parser<'a, Input, 'S> = string str >>. ret value

let inline charReturn ch value: Parser<'a, Input, 'S> = char ch >>. ret value

let inline stringSkip str: Parser<unit, Input, 'S> = stringReturn str ()

let inline charSkip str: Parser<unit, Input, 'S> = stringReturn str ()


let inline charsTake (predicate: char -> bool): Parser<string, Input, 'S> =
    many (pop () |> Parser.where predicate) |>> String.Concat
    //Parser.parser (fun (input, state) ->
    //    let mutable input = input
    //    let mutable charsReversed = Some []

    //    while Option.isSome charsReversed do
    //        charsReversed <-
    //            Input.tryGet input 
    //            |> Option.filter predicate
    //            |> Option.map (fun ch -> ch :: Option.get charsReversed)
            
    //        if Option.isSome charsReversed then
    //            input <- Input.advance 1 input
            
    //    let chars = Option.get charsReversed |> Seq.rev
    //    System.String.Concat chars, input, state)


let inline newline<'S> : Parser<string, Input, 'S> =
    choice [ string "\r\n"
             string "\n"
             string "\r" ]

let inline spacesln<'S> : Parser<string, Input, 'S> = 
    charsTake Char.IsWhiteSpace 

let inline spaces<'S> : Parser<string, Input, 'S> =
    charsTake (fun c -> Char.IsWhiteSpace c && c <> '\n' && c <> '\r')

//let inline surroundedSpaces parser = spaces >>. parser //.>> spaces
//let inline surroundedSpacesln parser = spacesln >>. parser .>> spacesln
