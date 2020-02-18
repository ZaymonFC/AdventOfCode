module AoCEighteen.Core
open System

module Files =
    let loadData fileName = System.IO.File.ReadLines(sprintf "%s.txt" fileName) |> List.ofSeq


module Input =
    let split (d: char) (s: string) = s.Split(d) |> List.ofArray
    let splitByCommas = split ','

    let parseInt = System.Int32.Parse
    let parseDouble = System.Double.Parse

    let parseIntsWithDelimeter d = split d >> List.map parseInt
    let parseIntsSeparatedByComma = parseIntsWithDelimeter ','

module PreMade =
    let loadData = Files.loadData
    let loadDataAndParseInts = Files.loadData >> (List.map Input.parseInt)
    let loadDataAndParseFloats = Files.loadData >> (List.map Input.parseDouble)

