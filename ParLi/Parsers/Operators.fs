[<AutoOpen>]
module ParLi.Parsers.Operators

[<AutoOpen>]
module BasicOperators =
    /// Contains overloads for all parser types (currently only 2 but may change)
    type Operators =
        //  Always null second argument because of overloading semantics
        static member Ret (value: 'a, _returnType: Parser<'a, 'T, 'S>): Parser<'a, 'T, 'S> = Parser.ret value
        static member Ret (value: 'a, _returnType: MaybeParser<'a, 'T, 'S>): MaybeParser<'a, 'T, 'S> = MaybeParser.ret value

        static member Map (x, f) = Parser.map f x
        static member Map (x, f) = MaybeParser.map f x

        static member Bind (x, f) = Parser.bind f x
        static member Bind (x, f) = MaybeParser.bind f x

        static member Then (x, y) = Parser.andThen x y
        static member Then (x, y) = MaybeParser.andThen x y
        static member Then (x, y) = MaybeParser.andThen (MaybeParser.some x) y
        static member Then (x, y) = MaybeParser.andThen x (MaybeParser.some y)

        static member ThenFst (x, y) = Parser.andThenFst x y
        static member ThenFst (x, y) = MaybeParser.andThenFst x y
        static member ThenFst (x, y) = MaybeParser.andThenFst (MaybeParser.some x) y
        static member ThenFst (x, y) = MaybeParser.andThenFst x (MaybeParser.some y)

        static member ThenSnd (x, y) = Parser.andThenSnd x y
        static member ThenSnd (x, y) = MaybeParser.andThenSnd x y
        static member ThenSnd (x, y) = MaybeParser.andThenSnd (MaybeParser.some x) y
        static member ThenSnd (x, y) = MaybeParser.andThenSnd x (MaybeParser.some y)

        static member Or (x, y) = MaybeParser.orElse x y
        static member Or (x, y) = MaybeParser.defaultWith x y

        static member Parse (x, input, state) = Parser.parseWith x input state
        static member Parse (x, input, state) = MaybeParser.parseWith x input state
    

    /// a parser that always returns this value
    let inline ret (value: 'a): '``Parser<'a, 'T, 'S>`` = 
        let inline call (_helper: ^Operators, source: ^A, output: ^B) =
            ( (^Operators or ^B) : (static member Ret: _ * _ -> _) source, output)
        call (Unchecked.defaultof<Operators>, value, Unchecked.defaultof<'``Parser<'a, 'T, 'S>``>)


    /// map the output of a parser
    let inline map (mapping: 'a -> 'b) (parser: '``Parser<'a, 'T, 'S>``): '``Parser<'b, 'T, 'S>`` = 
        let inline call (_helper: ^Operators, source: ^A, output: ^B) =
            ( (^Operators or ^A or ^B) : (static member Map: _ * _ -> _) source, mapping)
        call (Unchecked.defaultof<Operators>, parser, Unchecked.defaultof<'``Parser<'b, 'T, 'S>``>)

    /// map the output of a parser
    let inline (|>>) (x: '``Parser<'a, 'T, 'S>``) (f: 'a -> 'b): '``Parser<'b, 'T, 'S>`` = map f x


    /// bind the output of a parser
    let inline bind (binding: 'a -> '``Parser<'b, 'T, 'S>``) (parser: '``Parser<'a, 'T, 'S>``): '``Parser<'c, 'T, 'S>`` = 
        let inline call (_helper: ^Operators, source: ^A, output: ^B) =
            ( (^Operators or ^A or ^B) : (static member Bind: _ * _ -> _) source, binding)
        call (Unchecked.defaultof<Operators>, parser, Unchecked.defaultof<'``Parser<'c, 'T, 'S>``>)

    /// bind the output of a parser
    let inline (>>=) (x: '``Parser<'a, 'T, 'S>``) (f: 'a -> '``Parser<'b, 'T, 'S>``): '``Parser<'c, 'T, 'S>`` = bind f x


    /// return the output of parser1 or parser2
    let inline orElse (parser1: 'P1) (parser2: 'P2): 'Q = 
        let inline call (_helper: ^Operators, x: ^A, y: ^B, _output: ^C) =
            ( (^Operators or ^A or ^B or ^C) : (static member Or: _ * _ -> _) x, y)
        call (Unchecked.defaultof<Operators>, parser1, parser2, Unchecked.defaultof<'Q>)

    /// return the output of parser1 or parser2
    let inline (<|>) (x: '``Parser<'a, 'T, 'S>``) (y: '``Parser<'b, 'T, 'S>``): '``Parser<'c, 'T, 'S>`` = orElse x y
    
    /// return the output of parsing with parser1 and then parser2
    let inline andThen (parser1: 'P1) (parser2: 'P2): 'Q = 
        let inline call (_helper: ^Operators, x: ^A, y: ^B, _output: ^C) =
            ( (^Operators or ^A) : (static member Then: _ * _ -> _) x, y)
        call (Unchecked.defaultof<Operators>, parser1, parser2, Unchecked.defaultof<'Q>)

    /// return the output of parsing with parser1 and then parser2
    let inline andThenFst (parser1: 'P1) (parser2: 'P2): 'Q = 
        let inline call (_helper: ^Operators, x: ^A, y: ^B, _output: ^C) =
            ( (^Operators or ^A) : (static member ThenFst: _ * _ -> _) x, y)
        call (Unchecked.defaultof<Operators>, parser1, parser2, Unchecked.defaultof<'Q>)

    /// return the output of parsing with parser1 and then parser2
    let inline andThenSnd (parser1: 'P1) (parser2: 'P2): 'Q = 
        let inline call (_helper: ^Operators, x: ^A, y: ^B, _output: ^C) =
            ( (^Operators or ^A) : (static member ThenSnd: _ * _ -> _) x, y)
        call (Unchecked.defaultof<Operators>, parser1, parser2, Unchecked.defaultof<'Q>)

    /// return the output of parsing with parser1 and then parser2
    let inline (.>>.) (x: '``Parser<'a, 'T, 'S>``) (y: '``Parser<'b, 'T, 'S>``): '``Parser<'a * 'b, 'T, 'S>`` = andThen x y
    let inline (.>>) (x: '``Parser<'a, 'T, 'S>``) (y: '``Parser<'b, 'T, 'S>``): '``Parser<'a * 'b, 'T, 'S>`` = andThenFst x y
    let inline (>>.) (x: '``Parser<'a, 'T, 'S>``) (y: '``Parser<'b, 'T, 'S>``): '``Parser<'a * 'b, 'T, 'S>`` = andThenSnd x y

    //let inline (.>>.) x y: Parser<'a * 'b, 'T, 'S> = 
    //    Parser.andThen x y

    //let inline (.>>) x y: Parser<'a, 'T, 'S> = 
    //    Parser.andThenFst x y

    //let inline (>>.) x y: Parser<'b, 'T, 'S> = 
    //    Parser.andThenSnd x y

    //let inline (?>>?) (x: '``Parser<'a, 'T, 'S>``) (y: '``Parser<'b, 'T, 'S>``): MaybeParser<'a * 'b, 'T, 'S> = 
    //    let inline call (_operators: ^M, p1: ^A, p2: ^B) =
    //        ((^M or ^A or ^B) : (static member Then: _ * _ -> _) p1, p2)

    //    call (Unchecked.defaultof<Operators>, x, y)

    //let inline (?>>) (x: '``Parser<'a, 'T, 'S>``) (y: '``Parser<'b, 'T, 'S>``): MaybeParser<'a, 'T, 'S> = 
    //    x ?>>? y |>> fst

    //let inline (>>?) (x: '``Parser<'a, 'T, 'S>``) (y: '``Parser<'b, 'T, 'S>``): MaybeParser<'b, 'T, 'S> = 
    //    x ?>>? y |>> snd


    /// return the output of applying the parser on the input and state
    let inline parseWith (parser: '``Parser<'a, 'T, 'S>``) (input: 'T) (state: 'S) = 
        let inline call (_helper: ^Operators, parser: ^A) =
            ( (^Operators or ^A) : (static member Parse: _ * _ * _ -> _) parser, input, state)
        call (Unchecked.defaultof<Operators>, parser)


[<AutoOpen>]
module CompoundOperators = 

    let inline sequential (parsers: '``Parser<'a, 'T, 'S>`` list): '``Parser<'a list, 'T, 'S>`` = 
        let firstParser, parsers =
            match parsers with
            | [] -> failwithf "sequential cannot be called with []"
            | h :: t -> h, t        
        
        let mutable ret: '``Parser<'a list, 'T, 'S>`` = map List.singleton firstParser

        for parser in parsers do
            ret <- ret .>>. parser |>> fun (prev, p) -> p :: prev

        map List.rev ret


    let inline choice parsers = 
        let firstParser, parsers =
            match parsers with
            | [] -> failwithf "choice cannot be called with []"
            | h :: t -> h, t     

        let mutable ret = firstParser
        
        for parser in parsers do
            ret <- MaybeParser.orElse ret parser
        
        ret
