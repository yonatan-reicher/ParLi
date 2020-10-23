module ParLi.Linear.Position


let position i = 
    assert (i >= 0)
    Position i

let index (Position i) = i

/// A position object pointing at the start of the input
let start = position 0

/// empty = start. For consistency with other modules
let empty = start

/// Advance the position by an amount. i must be non-negative
let advance i (Position p) = 
    assert (i >= 0)
    Position(p + i)
    