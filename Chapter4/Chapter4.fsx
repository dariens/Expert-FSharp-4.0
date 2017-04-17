/// Chapter 4

/// IMperative Looping and Iterating

/// Simple for Loops

#load "../Chapter3/Chapter3.fsx"
open Chapter3

let repeatFetch url n =
    for i = 1 to n do
       let html = http url
       printfn "fetched <<< %s >>>" html
    printfn "Done!"

/// Simple While Loops

open System

let loopUntilSaturday() =
    while (DateTime.Now.DayOfWeek <> DayOfWeek.Saturday) do
        printfn "Still working!"
    printfn "Saturday at last!"

/// More Iteration Loops over Sequences
for (b, pj) in [("Banana 1", false); ("Banana 2", true)] do
    if pj then
       printfn "%s is in pyjamas today!" b


open System.Text.RegularExpressions

for m in Regex.Matches("All the Pretty Horses", "[a-zA-Z]+") do
    printfn "res = %s" m.Value

type DiscreteEventCounter =
    { mutable Total : int;
      mutable Positive : int;
      Name : string }

let recordEvent (s: DiscreteEventCounter) isPositive =
    s.Total <- s.Total + 1
    if isPositive then s.Positive <- s.Positive + 1

let reportStatus (s: DiscreteEventCounter) =
    printfn "We have %d %s out of %d" s.Positive s.Name s.Total

let newCounter nm =
    { Total = 0;
      Positive = 0;
      Name = nm }

let longPageCounter = newCounter "long page(s)"

let fetch url =
    let page = http url
    recordEvent longPageCounter (page.Length > 10000)
    page

fetch "http://www.smh.com.au" |> ignore

reportStatus longPageCounter

fetch "http://www.theage.com.au" |> ignore

reportStatus longPageCounter

/// Using Mutable let Bindings
let mutable cell1 = 1

cell1 <- 3

cell1

let sum n m =
    let mutable res =  0
    for i = n to m do
        res <- res + i
    res

sum 2 6


/// Hiding Mutable Data

let generateStamp =
    let mutable count = 0
    (fun () -> count <- count + 1; count)

generateStamp()

/// Working with Arrays

let arr = [|1.0; 1.0; 1.0|]

arr.[1]

arr.[1] <- 3.0

arr

let newArr = Array.append arr [|2.0;2.0|]

let coppiedArr = Array.copy newArr

coppiedArr.[0] <- 3.0

newArr = coppiedArr

Array.iter (fun x -> printfn "%A" x) newArr


/// Generating and Slicing Arrays
let interestingArr = [|for i in 0 .. 5 -> (i, i *i)|]

interestingArr.[1..3]
interestingArr.[..3]
interestingArr.[3..]
interestingArr.[3..] <- [|(1,1);(1,1);(1,1)|]
interestingArr

let arr2 = interestingArr

arr2.[0] <- (1,1)

arr2

interestingArr  // Changed because arr2 and interestingArr reference is the same


/// Introducing the Imperative .NET Collections

/// Using Resizable Arrays

let names = new ResizeArray<string>()

for name in ["Claire"; "Sophie"; "Jane"] do
    names.Add(name)

names.Count

for name in names do
    printfn "%s" name

names.[0]

let squares = new ResizeArray<int>(seq {for i in 0..100 -> i * i})

for x in squares do
    printfn "Square: %d" x

/// Using Dictionaries

open System.Collections.Generic

let capitals = new Dictionary<string, string>(HashIdentity.Structural)

capitals.["USA"] <- "Washington D.C."
capitals.["Bangladesh"] <- "Dhaka"
capitals.ContainsKey("USA")

capitals.["Bangladesh"]

for kvp in capitals do
    printfn "%s has capital %s" kvp.Key kvp.Value

/// Using Dictionary's TryGetValue
open System.Collections.Generic

let lookupName nm (dict: Dictionary<string, string>) =
    let mutable res = ""
    let foundIt = dict.TryGetValue(nm, &res)
    if foundIt then res
    else failwithf "Didn't find %s" nm

lookupName "USA" capitals

lookupName "Australia" capitals

lookupName "Dhaka" capitals

let mutable res = ""
capitals.TryGetValue("Australia", &res)  // false
res  // null
capitals.TryGetValue("USA", &res)  // true
res  // "Washington D.C."
capitals.TryGetValue("Bangladesh", &res)  //true
res  // "Dhaka"

capitals.TryGetValue("Australia")
capitals.TryGetValue("USA")

/// Using Dictionaries with Compound Keys

open System.Collections.Generic

open FSharp.Collections

let sparseMap = new Dictionary<(int* int), float>()

sparseMap.[(0,2)] <- 4.0

sparseMap.[(1021,1847)] <- 9.0

sparseMap.Keys

sparseMap


/// Exceptions and Controlling Them

let req = System.Net.WebRequest.Create("not a URL")

if false then 3 else failwith "hit the wall"

if (System.DateTime.Now > failwith "not yet decided") then
    printfn "you've run out of time!"


/// Catching Exceptions
open Microsoft.FSharp.Core

try
    raise (System.InvalidOperationException ("it's just not my day"))
with
    :? System.InvalidOperationException -> printfn "caught!"

open System.IO

exception BlockedURL of string

let http (url : string) =
    try
        if url = "http://www.kaos.org"
        then raise (BlockedURL(url))
        let req = System.Net.WebRequest.Create(url)
        let resp = req.GetResponse()
        let stream = resp.GetResponseStream()
        let reader = new StreamReader(stream)
        let html = reader.ReadToEnd()
        html
    with
        | :? System.UriFormatException -> ""
        | :? System.Net.WebException -> ""
        | BlockedURL url -> sprintf "blocked! url = '%s'" url

http "invalid URL"
http "http://www.kaos.org"
http "http://www.facebook.com"

try 
    raise (new System.InvalidOperationException ("invalid operation"))
with
    err -> printfn "oops, msg = '%s'" err.Message

/// Using try ... finally

let httpViaTryFinally (url: string) =
    let req = System.Net.WebRequest.Create(url)
    let resp = req.GetResponse()
    try
        let stream = resp.GetResponseStream()
        let reader = new StreamReader(stream)
        let html = reader.ReadToEnd()
        html
    finally
        resp.Close()


/// Defining New Exception Types


let http2 url =
    if url = "http://www.kaos.org"
    then raise (BlockedURL(url))
    else http url

/// Having an Effect: Basic I/O
open System.IO

let tmpFile = Path.Combine(__SOURCE_DIRECTORY__, "temp.txt")
File.WriteAllLines(tmpFile, [|"This is a test file."; "It is easy to read."|])

let tmpFileText = File.ReadAllLines tmpFile

for line in tmpFileText do
    printfn "%A" line

seq { for line in File.ReadLines tmpFile do
          let words = line.Split[|' '|]
          if words.Length > 3 && words.[2] = "easy" then
              yield line}

let outp = File.CreateText "playlist.txt"

outp.WriteLine "Enchanted"
outp.WriteLine "Put your records on"
outp.Close()

let inp = File.OpenText("playlist.txt")

inp.ReadLine()

inp.ReadLine()

inp.Close()

/// USing System.Console

System.Console.WriteLine "Hello World"

let consoleString = System.Console.ReadLine()

consoleString







