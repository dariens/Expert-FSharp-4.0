seq {0 .. 2}
seq {-100.0 .. 100.0}
let mySeq = seq {1..1000000}
mySeq |> Seq.map (fun v -> v * 2)

/// Iterating a Sequence

let range = seq {0 .. 2 .. 6}

for i in range do printfn "i = %d" i

/// Transforming Sequences with Functions

range |> Seq.map (fun i -> (i, i * i))

/// Using Lazy Sequences from External Sources

open System
open System.IO

let rec allFiles dir =
    Seq.append
        (dir |> Directory.GetFiles)
        (dir |> Directory.GetDirectories |> Seq.map allFiles |> Seq.concat)

allFiles Environment.SystemDirectory


/// Using Sequence Expressions

let squares = seq { for i in 0 .. 10 -> (i, i * i) }

seq {for (i, iSquared) in squares -> (i, iSquared, i * iSquared)}

/// Enriching Sequence Expressions with Additional Logic

let checkerboardCoordinates n =
    seq {for row in 1 .. n do
            for col in 1 .. n do
                let sum = row + col
                if sum%2 = 0 then
                    yield (row, col)}

checkerboardCoordinates 3

let fileInfo dir =
    seq { for file in Directory.GetFiles dir do
              let creationTime = File.GetCreationTime file
              let lastAccessTime = File.GetLastAccessTime file
              yield (file, creationTime, lastAccessTime)}

fileInfo Environment.SystemDirectory

let rec allFilesInfo dir =
    seq {for file in Directory.GetFiles dir do
            yield file
         for subdir in Directory.GetDirectories dir do
             yield! allFiles subdir}

allFilesInfo Environment.SystemDirectory
         
/// More on Working with Sequences

// A table of people in our startup
let people =
    [("Amber", 28, "Design")
     ("Wendy", 35, "Events")
     ("Antonia", 40, "Sales")
     ("Petra", 31, "Design")
     ("Carlos", 34, "Marketing")]

// Extract information from the table of people

let namesOfPeopleStartingWithA =
    people
      |> Seq.filter (fun (name, _age, _dept) -> name.StartsWith "A")
      |> Seq.toList

let namesOfDesigners =
    people
    |> Seq.filter (fun (_name, _age, dept) -> dept = "Design")
    |> Seq.map (fun (name, _age, _dept) -> name)
    |> Seq.toList