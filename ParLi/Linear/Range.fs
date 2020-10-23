module ParLi.Linear.Range


/// Create a range
let inline range start length = Range(start, length)

/// Create a range from 2 positions
let ofPositions (startPostion: Position) (endPosition: Position): Range =
    let dif = Position.index endPosition - Position.index startPostion
    range startPostion (int dif)

/// Create a range of 0 length
let point position = range position 0 
