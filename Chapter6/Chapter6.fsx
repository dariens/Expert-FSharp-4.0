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






