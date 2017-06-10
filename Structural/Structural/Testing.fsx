#r @"C:\Users\user\Documents\CODE\F#\Expert F# 4.0\Structural\Structural\bin\Debug\Structural.dll"
#r @"C:\Users\darien.shannon\Documents\Code\F#\Expert-FSharp-4.0\Structural\Structural\bin\Debug\Structural.dll"
open Structural
open StructuralShapes
open StructuralShapesOps

#load @"C:\Users\user\Documents\CODE\F#\Expert F# 4.0\Structural\Structural\Materials.fsx"
#load @"C:\Users\user\Documents\CODE\F#\Expert F# 4.0\Structural\Structural\ShapesDatabase.fsx"

#load @"C:\Users\darien.shannon\Documents\Code\F#\Expert-FSharp-4.0\Structural\Structural\Materials.fsx"
#load @"C:\Users\darien.shannon\Documents\Code\F#\Expert-FSharp-4.0\Structural\Structural\ShapesDatabase.fsx"

let myShape = Materials.AISC.SingleAngles.``L2x2x1/4``
let myShape2 = ShapesDatabase.AISC.SingleAngles.``L2x2x1/4``

let myArea = area myShape
