module ParLi.Text.Input

open ParLi.Linear


let inline ofString str: Input = Input.ofValue str

let inline rest (input: Input) = 
    let i = Position.index (Input.position input)
    (Input.value input).[i..]
