namespace Structural

open StructuralShapes
open Structural.StructuralShapesOps

module Testing =

    let mySingleAngle = SingleAngle {verticalLeg = 3.0<inch>;
                                     horizontalLeg = 3.0<inch>;
                                     thickness = 0.25<inch>}

     
        

    let myDoubleAngle = DoubleAngle {verticalLeg = 3.0<inch>;
                                     horizontalLeg = 3.0<inch>;
                                     thickness = 0.25<inch>;
                                     gap = 1.0<inch>}

    let myCC_SingleAngle = CC_SingleAngle {verticalLeg = 3.0<inch>;
                                           horizontalLeg = 3.0<inch>;
                                           thickness = 0.25<inch>;
                                           radius = 0.3125<inch>}

    let myCC_DoubleAngle = CC_DoubleAngle {verticalLeg = 3.0<inch>;
                                           horizontalLeg = 3.0<inch>;
                                           thickness = 0.25<inch>;
                                           radius = 0.3125<inch>;
                                           gap = 1.0<inch>}

    let myShapes = [mySingleAngle;myDoubleAngle;myCC_SingleAngle;myCC_DoubleAngle]

    myShapes 
    |> List.map description
    |> List.iter (fun s -> printfn "%s" s)

    let myAngle = createSingleAngle 2.0<inch> 2.0<inch> 0.25<inch>

    



                                                   



                            



