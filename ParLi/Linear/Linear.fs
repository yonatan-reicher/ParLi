[<AutoOpen>]
module ParLi.Linear.Main


/// Position of an element in any space (start of word in text, for example)
[<Struct>]
type Position = Position of int

/// Span of an element in any space (whole word in text, for example)
[<Struct>]
type Range = Range of start: Position * length: int

[<Struct>]
type 'T Input = Input of 'T * Position