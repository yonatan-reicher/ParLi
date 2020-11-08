[<AutoOpen>]
module ParLi.Arrays.Parsers

open ParLi.Parsers
open ParLi.Linear

let inline get<'t, 'S> : Parser<'t, 't array Input, 'S> = Parsers.get ()
let inline pop<'t, 'S> : Parser<'t, 't array Input, 'S> = Parsers.pop ()

