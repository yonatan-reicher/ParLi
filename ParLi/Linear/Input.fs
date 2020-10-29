module ParLi.Linear.Input

open System.Collections.Generic


/// Get the value of the input
let value (Input (a, _)) = a

/// The position the text is pointing at
let position (Input (_, position)) = position

/// Create an input of a value pointing at the start
let ofValue a = Input(a, Position.start)

/// Advance the position by a certain amount (must be non-negative)
let advance amount (Input (a, position)) =
    Input(a, Position.advance amount position)

let withPosition position (Input (a, _)) = Input(a, position)

/// Return the maximum position possible
let inline maxPosition (Input (a, _)) =
    Position.position (^a: (member Length: int) a)

/// Returns true if the position is inside the text (i.e. not EOF)
let inline inRange (Input (_, pos) as inp) = pos < maxPosition inp

/// Returns false if the position is inside the text (i.e. is EOF)
let inline eof inp: bool = not (inRange inp)

/// Matches if and only if is eof
let inline (|EOF|_|) text = if eof text then Some() else None


/// Class with overloads for for different operations on types 'T that
/// may be part of a 'T Input
type InputGenericsHelper =
    //  Some notes on making overloads that work with f# type constraints:
    //  Functions must have 1 or more type parameters (for example dont do static member f: string -> char)
    //  Functions should have sealed types only as parameters and returns (bad example: static member f: IList<'a> -> 'a)
    static member inline Index(array: 'T [], index: int, _) = array.[index]
    static member inline Index(lst: 'T list, index: int, _) = lst.[index]
    static member inline Index(str: string, index: int, _) = str.[index]
    //static member inline Index(lst: ^A, index: int, _) = (^A: (static member Item: _ * _ -> _) lst, index)

/// Returns the element of 'T at the current position in the input
/// 'T must be one of: string, 'T list, 'T array
let inline get (Input (value: 'T, Position i)) : 'a =
    let inline call (_mthd: ^M, value: ^A) =
        ((^M or ^A) : (static member Index: _ * _ * _ -> _) value, i, null)
    call (Unchecked.defaultof<InputGenericsHelper>, value)
    
/// Same as writing
///     match input with EOF -> None | _ -> get input
/// note: expects 'a.Length and 'a.Index to be implemented correctly
let inline tryGet (input: 'T Input) =
    match input with
    | EOF -> None
    | _ -> Some(get input)
