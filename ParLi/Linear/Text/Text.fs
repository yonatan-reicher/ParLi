[<AutoOpen>]
module ParLi.Linear.Text.Main

open System.IO

open ParLi
open ParLi.Linear

let string (str: string): MaybeParser<string, string Input, 'S> =
    MaybeParser.parser (fun (input, state) ->
        let (Input (inputString, Position i)) = input
        let len = str.Length

        if inputString.[i..(i + len)] = str then
            Some str, Input.advance len input, state
        else
            None, input, state)
