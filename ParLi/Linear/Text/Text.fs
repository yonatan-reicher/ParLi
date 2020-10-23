[<AutoOpen>]
module ParLi.Linear.Text.Main

open System.IO

open ParLi
open ParLi.Linear

open MaybeParser


let inline string (str: string): MaybeParser<string, string Input, 'S> =
    parser (fun (input, state) ->
        let (Input (inputString, Position i)) = input
        let len = str.Length

        if inputString.[i..(i + len - 1)] = str then
            Some str, Input.advance len input, state
        else
            None, input, state)

let inline char (ch: char): MaybeParser<char, string Input, 'S> =
    parser (fun (input, state) ->
        let (Input (inputString, Position i)) = input

        if inputString.[i] = ch then
            Some ch, Input.advance 1 input, state
        else
            None, input, state)


let inline stringReturn str value = string str >>. ret value

let inline charReturn ch value = char ch >>. ret value

let inline stringSkip str = stringReturn str ()

let inline charSkip str = stringReturn str ()


let inline newline<'S> : MaybeParser<string, string Input, 'S> =
    choice [ string "\r\n"
             string "\n"
             string "\r" ]

let inline eof<'S> : MaybeParser<unit, string Input, 'S> =
    parser (fun (input, state) -> Input.``|EOF|_|`` input, input, state)
