[<AutoOpen>]
module ParLi.Text.Parsers

open System
open ParLi.Parsers
open ParLi.Linear

let inline char (ch: char): MaybeParser<char, Input, 'S> =
    pop () |> MaybeParser.where ((=) ch)

let inline string (str: string): MaybeParser<string, Input, 'S> =
    MaybeParser.maybeParser (fun (input, state) ->
        let (Input (inputString, Position i)) = input
        let len = str.Length

        if inputString.[i..(i + len - 1)] = str then
            Some str, Input.advance len input, state
        else
            None, input, state)

let inline stringReturn str value: MaybeParser<'a, Input, 'S> = MaybeParser.andThenSnd (string str) (MaybeParser.ret value: MaybeParser<'a, Input, 'S>)

let inline charReturn ch value: MaybeParser<'a, Input, 'S> = MaybeParser.andThenSnd (char ch) (MaybeParser.ret value: MaybeParser<'a, Input, 'S>)

let inline stringSkip str: MaybeParser<unit, Input, 'S> = stringReturn str ()

let inline charSkip str: MaybeParser<unit, Input, 'S> = stringReturn str ()


let inline charsTake (predicate: char -> bool): Parser<string, Input, 'S> =
    MaybeParser.many (pop () |> MaybeParser.where predicate) |>> String.Concat
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


let inline newline<'S> : MaybeParser<string, Input, 'S> =
    choice [ string "\r\n"
             string "\n"
             string "\r" ]

let inline spacesln<'S> : Parser<string, Input, 'S> = 
    charsTake Char.IsWhiteSpace 

let inline spaces<'S> : Parser<string, Input, 'S> =
    charsTake (fun c -> Char.IsWhiteSpace c && c <> '\n' && c <> '\r')

//let inline surroundedSpaces parser = spaces >>. parser //.>> spaces
//let inline surroundedSpacesln parser = spacesln >>. parser .>> spacesln
