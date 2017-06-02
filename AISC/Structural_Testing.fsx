System.Environment.CurrentDirectory <- __SOURCE_DIRECTORY__
#load @"Structural.fsx"
#load @"Shapes.fsx"
#load @"Materials.fsx"

open Structural
open Shapes
open Materials

let myAngle1 = SingleAngle {horizontalLeg = 3.0<inch>;
                            verticalLeg = 3.0<inch>;
                            thickness = 0.25<inch>;
                            radius = 0.0<inch>}

let myAngle2 = SingleAngle (SingleAngle.Create 3.0<inch> 3.0<inch> 0.25<inch> 0.0<inch>)

myAngle1 = myAngle2

let myDoubleAngle = DoubleAngle{horizontalLeg = 3.0<inch>;
                                verticalLeg = 3.0<inch>;
                                thickness = 0.25<inch>;
                                radius = 0.0<inch>;
                                gap = 1.0<inch>}

    


printfn @"%s Area : %f
%s Area : %f" myAngle2.Description myAngle2.Area myDoubleAngle.Description myDoubleAngle.Area
        
  
let twoByTwoByQuarter = AISC.SingleAngles.``L2x2x1/4``

let myMaterial = SteelMaterial {fy = 50.0<ksi>; fu = 60.0<ksi>; E = 29000.0<ksi>}

let mySection1 =  { shape = twoByTwoByQuarter;
                    material = myMaterial}




/// Simplest of the tests :


let mySection2 = {shape = AISC.SingleAngles.``L2x2x1/4``;
                  material = ASTM.ASTM1}


printfn "%A" (mySection2.AllowableTension.ToString())

let myResult = mySection2.AllowableTension.Value

let mySection3 = {shape = AISC.DoubleAngles.``2L2x2x1/4``;
                  material = ASTM.ASTM1}

printfn "%A" (mySection3.AllowableTension.ToString())
