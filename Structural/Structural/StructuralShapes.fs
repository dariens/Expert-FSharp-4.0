namespace Structural

open System
open System.Collections.Generic

[<Measure>] type inch
[<Measure>] type lbf

module StructuralShape =


    type SingleAngle = {verticalLeg : float<inch>;
                        horizontalLeg : float<inch>;
                        thickness : float<inch>}

    type DoubleAngle = {verticalLeg : float<inch>;
                        horizontalLeg : float<inch>;
                        thickness : float<inch>;
                        gap : float<inch>}
                    
    type CC_SingleAngle = {verticalLeg : float<inch>;
                           horizontalLeg : float<inch>;
                           thickness : float<inch>;
                           radius : float<inch>}

    type CC_DoubleAngle = {verticalLeg : float<inch>;
                           horizontalLeg : float<inch>;
                           thickness : float<inch>;
                           radius : float<inch>;
                           gap : float<inch>}

    type StructuralShape =
        | SingleAngle of SingleAngle
        | DoubleAngle of DoubleAngle
        | CC_SingleAngle of CC_SingleAngle
        | CC_DoubleAngle of CC_DoubleAngle

        
        member ss.Description =
            match ss with
            | SingleAngle sa -> String.Format("L{0}x{1}x{2}",
                                              sa.verticalLeg, sa.horizontalLeg, sa.thickness)
            | DoubleAngle da -> String.Format("2L{0}x{1}x{2}x{3}",
                                              da.verticalLeg, da.horizontalLeg, da.thickness, da.gap)
            | CC_SingleAngle ccsa -> String.Format("CCL{0}x{1}x{2}r{3}",
                                         ccsa.verticalLeg, ccsa.horizontalLeg,
                                         ccsa.thickness, ccsa.radius)
            | CC_DoubleAngle ccda -> String.Format("CC2L{0}x{1}x{2}x{3}r{4}",
                                         ccda.verticalLeg, ccda.horizontalLeg,
                                         ccda.thickness, ccda.gap, ccda.radius)

        member ss.Area =
            match ss with
            | SingleAngle sa -> (sa.horizontalLeg + sa.verticalLeg - sa.thickness) * sa.thickness
            | DoubleAngle da -> 2.0 * (da.horizontalLeg + da.verticalLeg - da.thickness) * da.thickness
            | CC_SingleAngle ccsa -> ((ccsa.horizontalLeg - ccsa.radius) +
                                      (ccsa.verticalLeg - ccsa.radius) +
                                      (2.0 * Math.PI * ccsa.radius * 0.25)) * ccsa.thickness
            | CC_DoubleAngle ccda -> 2.0 * ((ccda.horizontalLeg - ccda.radius) +
                                            (ccda.verticalLeg - ccda.radius) +
                                            (2.0 * Math.PI * ccda.radius * 0.25)) * ccda.thickness

        member ss.xbar =
            match ss with
            | SingleAngle sa->
                (sa.horizontalLeg * sa.thickness * sa.horizontalLeg/2.0 +
                 ((sa.verticalLeg - sa.thickness) * sa.thickness * sa.thickness/2.0)) / (ss.Area)
            | DoubleAngle _ -> 0.0<inch>
            | CC_DoubleAngle _ -> 0.0<inch>

        member ss.ybar =
            match ss with
            | SingleAngle sa ->
                ((sa.horizontalLeg - sa.thickness) * sa.thickness * sa.thickness/2.0 +
                 (sa.verticalLeg * sa.thickness * sa.verticalLeg/2.0)) / (ss.Area)
            | DoubleAngle da ->
                ((da.horizontalLeg - da.thickness) * da.thickness * da.thickness/2.0 +
                 (da.verticalLeg * da.thickness * da.verticalLeg/2.0)) / (ss.Area / 2.0)


    let description (ss: StructuralShape) = ss.Description
    let area (ss: StructuralShape) = ss.Area
    let xbar (ss: StructuralShape) = ss.xbar
    let ybar (ss: StructuralShape) = ss.ybar
    


    
    

    

