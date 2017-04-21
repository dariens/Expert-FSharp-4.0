///Chapter 6 : Programming with Objects

///Getting Stated with Objects and Members

/// Two-dimensional vectors
type Vector2DOld =
    {DX : float; DY : float}
    /// Get the length of the vector
    member v.Length = sqrt(v.DX * v.DX + v.DY * v.DY)
    /// Return a vector scaled by the given factor
    member v.Scale k = {DX = k*v.DX; DY = k * v.DY}
    /// Return a vector shifted by the given delta in the X coordinate
    member v.ShiftX x = { v with DX = v.DX + x }
    /// Return a vector shifted by the given delta in the Y Coordinate
    member v.ShiftY y = { v with DY = v.DY + y }
    /// Return a vector shifted by the given distance in both coordinates
    member v.ShiftXY (x, y) = {DX = v.DX + x; DY = v.DY + y}
    /// Get the zero vector
    static member Zero = {DX = 0.0; DY = 0.0}
    /// Return a constant vector along the x axis
    static member ConstX dx = {DX = dx; DY = 0.0}
    /// Return a constant vector along the  y axis
    static member ConstY dy = { DX = 0.0; DY = dy}
    member v.LengthWithSideEffect =
        printfn "Computing!"
        sqrt(v.DX * v.DX + v.DY * v.DY)

let v = {DX = 3.0; DY = 4.0}

v.Length

v.Scale(2.0).Length

Vector2DOld.ConstX(3.0)

let newVector = {DX = 4.0; DY = 4.0}

newVector.LengthWithSideEffect

/// A type of binary tree, generic in the type of values carried at nodes
type Tree<'T> =
    | Node of 'T * Tree<'T> * Tree<'T>
    | Tip
    member t.Size =
        match t with
        | Node(_, l, r) -> 1 + l.Size + r.Size
        | Tip -> 0

///Using Classes

/// A vector 2D type as a class
type Vector2D(dx : float, dy : float) =
    let len = sqrt(dx * dx + dy * dy)
    static let zero = Vector2D(0.0, 0.0)
    static let oneX = Vector2D(1.0, 0.0)
    static let oneY = Vector2D(0.0, 1.0)
    member v.DX = dx
    member v.DY = dy
    member v.Length = len
    member v.Scale(k) = Vector2D(k * dx, k * dy)
    member v.ShiftX(x) = Vector2D(dx = dx + x, dy = dy)
    member v.ShiftY(y) = Vector2D(dx = dx, dy = dy + y)
    static member Zero = zero
    static member OneX = oneX
    static member OneY = oneY

let v2= Vector2D(3.0, 4.0)
let v3 = Vector2D.OneX

v2.Length
v2.Scale(2.0).Length

/// Vectors whose length is checked to be close to length one.
type UnitVector2D(dx,dy) =
    let tolerance = 0.000001

    let length = sqrt (dx * dx + dy * dy)
    do if abs (length - 1.0) >= tolerance then failwith "not a unit vector"

    member v.DX = dx
    member v.DY = dy
    new() = UnitVector2D (1.0, 0.0)
    


/// Adding Further Object Notation to Your Types

open System.Collections.Generic

type SparseVector(items : seq<int * float>) =
    let elems = new SortedDictionary<_,_>()
    do items |> Seq.iter (fun (k, v) -> elems.Add(k,v))

    ///This defines an indexer property
    member t.Item
        with get(idx) =
            if elems.ContainsKey(idx) then elems.[idx]
            else 0.0

let v5 = SparseVector [|(3, 547.0); (4, 550.0)|]

v5.[3]

///Adding Overloaded Operators

type Vector2DWithOperators(dx : float, dy : float) =
    member x.DX = dx
    member x.DY = dy

    static member (+) (v1: Vector2DWithOperators, v2: Vector2DWithOperators) =
        Vector2DWithOperators(v1.DX + v2.DX, v1.DY + v2.DY)

    static member (-) (v1: Vector2DWithOperators, v2: Vector2DWithOperators) =
        Vector2DWithOperators(v1.DX - v2.DX, v1.DY - v2.DY)

    member v.Print () = printfn "DX = %f; DY = %f" v.DX v.DY

let v6 = new Vector2DWithOperators(3.0, 4.0)
v6.Print ()
let v7 = v6 + v6
v7.Print ()

let (++) x y = List.append x [y]

let list = [1;2;3]

list ++ 4

/// Using Named and Optional Arguments

open System.Drawing

type LabelInfo(?text : string, ?font : Font) =
    let text = defaultArg text ""  
    let font = match font with
               | None -> new Font(FontFamily.GenericSansSerif, 12.0f)
               | Some v -> v
    member x.Text = text
    member x.Font = font

    static member Create(?text, ?font) = new LabelInfo(?text = text, ?font = font)

LabelInfo (text = "Hello World")
LabelInfo("Goodbye Lenin")
LabelInfo.Create()
LabelInfo(font = new Font(FontFamily.GenericMonospace, 36.0f),
          text = "Imagine")


/// Adding Method Overloading

/// Interval(lo,hi) represents the range of numbers from lo to hi,
/// but not including either lo or hi.

type Interval(lo, hi) =
    member r.Lo = lo
    member r.Hi = hi
    member r.IsEmpty = hi <= lo
    member r.Contains v = lo < v && v < hi

    static member Empty = Interval(0.0, 0.0)

    /// Return the smallest interval that covers both the intervals
    static member Span (r1 : Interval, r2 : Interval) =
        if r1.IsEmpty then r2 else
        if r2.IsEmpty then r1 else
        Interval (min r1.Lo r2.Lo, max r1.Hi r2.Hi)

    /// Return the smallest interval that covers all the intervals
    static member Span (ranges : seq<Interval>) =
        Seq.fold (fun r1 r2 -> Interval.Span(r1, r2)) Interval.Empty ranges

let interval1 = Interval(3.0, 6.0)
let interval2 = Interval(5.0, 9.0)
let interval3 = Interval (12.0, 14.0)
let intervalSeq = seq [interval1;interval2;interval3]
Interval.Span(interval1,interval2)
Interval.Span(intervalSeq)

type Vector =
    { DX : float; DY : float}
    member v.Length = sqrt (v.DX * v.DX + v.DY * v.DY)

type Point =
    { X : float; Y : float}

    static member (-) (p1 : Point, p2 : Point) =
        { DX = p1.X - p2.X; DY = p1.Y - p2.Y }

    static member (-) (p : Point, v : Vector) = 
        { X = p.X - v.DX; Y = p.Y - v.DY }

/// Defining Object Types with Mutable State



