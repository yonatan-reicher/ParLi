module ParLi.Linear.Input

/// Get the value of the input
let value (Input (a, _)) = a

/// The position the text is pointing at
let position (Input (_, position)) = position

/// Create an input of a value pointing at the start
let ofValue a = Input(a, Position.start)

/// Advance the position by a certain amount (must be non-negative)
let advance amount (Input (a, position)) = Input(a, Position.advance amount position)

let withPosition position (Input (a, _)) = Input (a, position)

/// Return the maximum position possible
let inline maxPosition (Input (a, _)) = Position.position (^a : (member Length: int) a)

/// Returns true if the position is inside the text (i.e. not EOF)
let inline inRange (Input (_, pos) as inp) = pos < maxPosition inp

/// Returns false if the position is inside the text (i.e. is EOF)
let inline eof inp: bool = not (inRange inp)

/// Matches if and only if is eof
let inline (|EOF|_|) text = if eof text then Some() else None
