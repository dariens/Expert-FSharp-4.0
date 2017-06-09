#r @"C:\Users\user\Documents\CODE\F#\Expert F# 4.0\Structural\Structural\bin\Debug\Structural.dll"
open Structural
open StructuralShapes
open StructuralShapesOps

module AISC =

    type SingleAngles =
        static member ``L2x2x1/4`` = createSingleAngle 2.0<inch> 2.0<inch> 0.25<inch>
        static member ``L3x3x1/4`` = createSingleAngle 3.0<inch> 3.0<inch> 0.25<inch>

    type DoubleAngles =
        static member ``2L2x2x1/4`` = createDoubleAngle 2.0<inch> 2.0<inch> 0.25<inch> 1.0<inch>
        static member ``2L3x3x1/4`` = createDoubleAngle 3.0<inch> 3.0<inch> 0.25<inch> 1.0<inch>


    



