/// Chapter 5: Understanding Types in Functional Programming

/// Exploring Some Simple Type Definitions

/// Defining Type Abbreviations

type index = int
type flags = int64
type results = string * System.TimeSpan * int * int

/// Defining Record Types
open System

type Person =
    { Name : string
      DateOfBirth : DateTime}

//{Name = "Bill"; DateOfBirth = DateTime(1962, 09, 02)}

type PageStats =
    { Site : string
      Time : System.TimeSpan
      Length : int
      NumWords : int
      NumHRefs : int}

#load "../Chapter3/Chapter3.fsx"
open Chapter3

let stats site =
    let url = "http://" + site
    let html, t = time (fun () -> http url)
    let words = html |> getWords
    let hrefs = words |> Array.filter (fun s -> s = "href")
    { Site = site
      Time = t
      Length = html.Length
      NumWords = words.Length
      NumHRefs = hrefs.Length}

//stats "www.google.com"


/// Handling Non-Unique Record Field Names

type Company =
    { Name : string
      Address : string}

type Dot = {X : int; Y : int}
type Point = {X : float; Y : float}


let coords1 (p:Point) = (p.X, p.Y)

let coords2 (d:Dot) = (d.X, d.Y)

let dist p = sqrt (p.X * p.X + p.Y * p.Y)  // inferes Point since it is the last defined type that matches


type Animal =
    { Name : string
      DateOfBirth :DateTime}

// Without explicit type annotation this would be inferred as type Animal 
// Since Animal was the last definition that fits.

let anna = ({Name = "Anna"; DateOfBirth = new System.DateTime(1968, 07, 23)} : Person)


/// Cloning Records

type Point3D = {X: float; Y: float; Z: float}

let p1 = {X = 3.0; Y = 4.0; Z = 5.0}
let p2 = {p1 with Y = 0.0; Z = 0.0}

/// Defining Discriminated Unions

type Route = int
type Make = string
type Model = string
type Transport =
    | Car of Make * Model
    | Bicycle
    | Bus of Route

let ian = Car("BMW", "360")
let don = [Bicycle; Bus 8]
let peter = [Car("Ford", "Fiesta"); Bicycle; Bus 10]

let averageSpeed (tr : Transport) =
    match tr with
    | Car ("BMW", _) -> 60
    | Car _ -> 65
    | Bicycle -> 16
    | Bus 2 | Bus 3 -> 32
    | Bus _ -> 24

type Proposition =
    | True
    | And of Proposition * Proposition
    | Or of Proposition * Proposition
    | Not of Proposition

let rec eval (p : Proposition) =
    match p with
    | True -> true
    | And (p1, p2) -> eval p1 && eval p2
    | Or  (p1, p2) -> eval p1 || eval p2
    | Not (p1) -> not (eval p1)

//eval True
//eval (And (True, Not True))
//eval (Or (True, Not True))

type Tree<'T> =
    | Tree of 'T * Tree<'T> * Tree<'T>
    | Tip of 'T

let rec sizeOfTree tree =
    match tree with
    | Tree(_, l, r) -> 1 + sizeOfTree l + sizeOfTree r
    | Tip _ -> 1

let smallTree = Tree("1", Tree("2", Tip "a", Tip "b"), Tip "c")

//sizeOfTree smallTree

/// Using Discriminated Unions as Records

type Point3D2 = Vector3D of float * float * float

let origin = Vector3D(0., 0., 0.)
let unitX = Vector3D(1., 0., 0.)
let unitY = Vector3D(0., 1., 0.)
let unitZ = Vector3D(0., 0., 1.)

let length (Vector3D(dx, dy, dz)) = sqrt (dx * dx + dy * dy + dz * dz)

/// Defining Multiple Types Simultaneously
type Node =
    { Name : string
      Links : Link list}
and Link =
    | Drangling
    | Link of Node

/// Understanding Generics


type StringMap<'T> = Map<string, 'T>

type Projections<'T, 'U> = ('T -> 'U) * ('U -> 'T)

let rec map (f: 'T -> 'U) (l : 'T list) =
    match l with
    | h :: t -> f h :: map f t
    | [] -> []

//[1..3] |> map (fun x -> x * x)

/// Writing Generic Functions

let getFirst (a,b,c) = a

let mapPair f g (x, y) = (f x, g y)


/// Some Important Generic Functions

//compare 3 2
//compare 'a' 'b'

//("abc", "def") < ("abc", "xyz")

//compare [10;30] [10;20]

/// Generic Hashing


//hash 100

//hash "abc"

//hash (100, "abc")

//hash (100.0)

//hash ([1;2;3])

/// Generic Pretty-Printing

//sprintf "result = %A" ([1], [true])

let darien = ({Name = "Darien"; DateOfBirth = new System.DateTime(1991, 01, 14)} : Person)

//sprintf "result = %A" darien


/// Generic Boxing and Unboxing

//box 1
//box "abc"
let stringObj = box "abc"
//(unbox<string> stringObj)
//(unbox stringObj : string)

let testFunc = (fun x -> x * x)

let boxedTestFunc = box testFunc

let unBoxedTestFunc = unbox<int -> int> boxedTestFunc

//unBoxedTestFunc 5

/// Generic Binary Serialization via the .NET Libraries

open System.IO
open System.Runtime.Serialization.Formatters.Binary

let writeValue outputStream x = 
    let formatter = new BinaryFormatter()
    formatter.Serialize(outputStream, box x)

let readValue inputStream =
    let formatter = new BinaryFormatter()
    let res = formatter.Deserialize(inputStream)
    unbox res

let addresses =
    Map.ofList ["Jeff", "123 Main Street, Redmond, WA 98052"
                "Fred", "987 Pine Road, Phila., PA 19116"
                "Mary", "PO Box 112233, Palo Alto, CA 94301"]

let fsOut = new FileStream(__SOURCE_DIRECTORY__ + "/Data.dat", FileMode.Create)
//writeValue fsOut addresses
//fsOut.Close()

let fsIn = new FileStream(__SOURCE_DIRECTORY__ + "/Data.dat", FileMode.Open)
let res : Map<string, string> = readValue fsIn
//fsIn.Close()


/// Using System.Runtime.Serialization.Json
#r "System.Runtime.Serialization.dll"
open System.Runtime.Serialization.Json
open System.Runtime.Serialization
open System.Runtime.Serialization.Formatters.Binary

[<DataContract>]
type Joist =
    {
        [<field : DataMember>]
        Mark : string

        [<field : DataMember>]
        TCSize : string
    }

let joist = {Mark = "J1"; TCSize = "3028"}


//let jsonOut = new FileStream(__SOURCE_DIRECTORY__ + "/Data6.txt", FileMode.Create)
//let jsonSerializer = new DataContractJsonSerializer(typedefof<Joist>)
//jsonSerializer.WriteObject(jsonOut, joist)
//jsonOut.Close()

//let jsonIn = new FileStream(__SOURCE_DIRECTORY__ + "/Data6.txt", FileMode.Open)
//let newJoist = jsonSerializer.ReadObject jsonIn :?> Joist
//jsonIn.Close()

/// Using Newtonsoft.Json.dll

#r "packages/Newtonsoft.Json/lib/net45/Newtonsoft.Json.dll"
open Newtonsoft.Json

type Joist2 =
    { Mark : string
      TCSize : string}

let joist1 = {Mark = "J1"; TCSize = "3028"}
let joist2 = {Mark = "J2"; TCSize = "4050"}
let joists = [joist1;joist2]

let json = JsonConvert.SerializeObject(joists, Formatting.Indented)
printfn "%A" json

type testUnion =
    | Car of string
    | Bus of string * int
    | Plane of string

let car = Car("BMW")
let bus = Bus("Bus", 5)
let plane = Plane("Plane")

let vehicles = [car; bus; plane]


let jsonCar = JsonConvert.SerializeObject(vehicles, Formatting.Indented)

let newJsonCar = jsonCar.Replace("BMW", "Mercedes")

let newCar = JsonConvert.DeserializeObject<testUnion list>(newJsonCar)

/// Using JsonConvert.DeserializeXMLNode to convert Json to XML
#r "System.Xml.Linq.dll"
open System.Xml.Linq
open System.Security.Cryptography

// Function to add object to beginning of json
let formatJson firstObject json =
    sprintf "{\"%s\": %s }" firstObject json

let formatedJsonCar = formatJson "TestUnion_List" newJsonCar

let node = JsonConvert.DeserializeXmlNode(formatedJsonCar, "root")
node.Save(__SOURCE_DIRECTORY__ + "/testXML.xml")
printfn "%A" node

/// Making Things Generic





