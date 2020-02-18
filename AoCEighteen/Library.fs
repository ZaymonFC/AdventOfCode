namespace AoCEighteen

open Core

module Day1 =
    module Part1 =
        let solution = PreMade.loadDataAndParseInts "day1" |> List.sum

    module Part2 =
        let recurringFrequency (input: int list) =
            let rec looper currentFrequency (history: int Set) remaining =
                match remaining with
                | [] -> looper currentFrequency history input
                | head :: tail ->
                    match history.Contains currentFrequency with
                    | true -> currentFrequency
                    | false ->
                        let nextFrequency = currentFrequency + head
                        looper nextFrequency (history |> Set.add currentFrequency) tail

            match input with
            | [] -> failwith "Empty"
            | head :: tail -> looper head (Set []) tail

        let solution = PreMade.loadDataAndParseInts "day1" |> recurringFrequency

module Day2 =
    type Frequency = (char * int) list

    module Frequency =
        let create() = [ 'a' .. 'z' ] |> List.map (fun c -> (c, 0))

        let insert freq (c: char) =
            freq
            |> List.map (fun (char, count) ->
                match char = c with
                | true -> (char, count + 1)
                | _ -> (char, count))

        let fromString (s: string): Frequency = List.fold insert (create()) (s |> List.ofSeq)

        let containsCount count freq = freq |> List.exists (fun (_, c) -> c = count)

    let data = PreMade.loadData "day2"

    // Part 1
    let freqs = data |> List.map Frequency.fromString

    let freq2 =
        freqs
        |> List.filter (fun freq -> freq |> Frequency.containsCount 2)
        |> List.length

    let freq3 =
        freqs
        |> List.filter (fun freq -> freq |> Frequency.containsCount 3)
        |> List.length

    let checkSum = freq2 * freq3

    // Part 2
    let hammingDistance x y =
        (x, y)
        ||> Seq.map2 (<>)
        |> Seq.sumBy (fun s ->
            if s then 1 else 0)

    let common (x: string) (y: string) =
        (x, y)
        ||> Seq.map2 (fun c1 c2 ->
                if c1 = c2 then Some c1 else None)
        |> Seq.choose id
        |> System.String.Concat


    let hammingDistanceOf1 =
        [ for x in data do
            for y in data do
                if hammingDistance x y = 1 then yield x ]

    let answer =
        match hammingDistanceOf1 with
        | [ head; tail ] -> common head tail
        | _ -> failwith "Ooops!"

module Day3 =
    open FParsec

    let tryRunParser p s =
        match run p s with
        | Success(result, _, _) -> Some result
        | _ -> None

    type Rect =
        { Top: int
          Left: int
          Bottom: int
          Right: int }

    type Claim =
        { Id: int
          Rectangle: Rect }

    type AnonymousParser<'a> = Parser<'a, unit>

    // Claim Format:
    // #123 @ 2,3: 5x5
    // Where 123 is the ID
    // 2,3 is the top left corner
    // and 5x5 is width x height

    let pTuple2Int: AnonymousParser<int * int> = pint32 .>> pchar ',' .>>. pint32
    let pHashId: AnonymousParser<int> = pchar '#' >>. pint32
    let pDimensions: AnonymousParser<int * int> = pint32 .>> pchar 'x' .>>. pint32
    let pAt: AnonymousParser<_> = pchar '@'
    let pAtWithSpaces: AnonymousParser<_> = spaces .>> pAt .>> spaces


    let claimParser: Parser<_, unit> =
        pHashId .>> pAtWithSpaces .>>. pTuple2Int .>> pchar ':' .>> spaces .>>. pDimensions

    let data = PreMade.loadData "day3"

    let structuredData =
        data
        |> List.map (tryRunParser claimParser)
        |> List.choose id
        |> List.map (fun ((claimId, (topLx, topLy)), (width, height)) ->
            { Id = claimId
              Rectangle =
                  { Top = topLy
                    Right = topLx + width
                    Bottom = topLy + height
                    Left = topLx } })

    let result = structuredData

    let overlap (r1: Rect) (r2: Rect) : bool =
        true
