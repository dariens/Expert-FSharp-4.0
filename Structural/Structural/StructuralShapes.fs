namespace Structural

open System
open System.Collections.Generic

[<Measure>] type inch

module StructuralShapes =

    type IStructuralShape = interface end

    type SingleAngle = {verticalLeg : float<inch>;
                        horizontalLeg : float<inch>;
                        thickness : float<inch>}

                        interface IStructuralShape

    type DoubleAngle = {verticalLeg : float<inch>;
                        horizontalLeg : float<inch>;
                        thickness : float<inch>;
                        gap : float<inch>}

                        interface IStructuralShape
    
    type Plate = {length : float<inch>;
                  thickness: float<inch>; }

                  interface IStructuralShape
                                  
    type CC_SingleAngle = {verticalLeg : float<inch>;
                           horizontalLeg : float<inch>;
                           thickness : float<inch>;
                           radius : float<inch>;}

                           interface IStructuralShape

                           member ccsa.Blank = 
                               {
                                   length = ((ccsa.horizontalLeg - ccsa.radius - ccsa.thickness) +
                                             (ccsa.verticalLeg - ccsa.radius - ccsa.thickness) +
                                             (2.0 * Math.PI * (ccsa.radius + ccsa.thickness/2.0) * 0.25));

                                   thickness = ccsa.thickness;
                                }

                           static member EqualLegFromBlank (blank: Plate) radius =
                                let radius' = radius + blank.thickness/2.0
                                let curveLength = 2.0 * Math.PI * radius' * 0.25
                                let legLength = (blank.length - curveLength) / 2.0
                                {verticalLeg = legLength;
                                 horizontalLeg = legLength;
                                 thickness = blank.thickness;
                                 radius = radius;}

                           static member UnequalLegFromBlankAndHLeg (blank: Plate) radius hLeg =
                                let radius' = radius + blank.thickness/2.0
                                let curveLength = 2.0 * Math.PI * radius' * 0.25
                                let vLeg = blank.length - curveLength - hLeg
                                {verticalLeg = vLeg;
                                 horizontalLeg = hLeg;
                                 thickness = blank.thickness;
                                 radius = radius;}

                           static member UnequalLegFromBlankAndVLeg (blank: Plate) radius vLeg =
                                let radius' = radius + blank.thickness/2.0
                                let curveLength = 2.0 * Math.PI * radius' * 0.25
                                let hLeg = blank.length - curveLength - vLeg
                                {verticalLeg = vLeg;
                                 horizontalLeg = hLeg;
                                 thickness = blank.thickness;
                                 radius = radius;}

                           member sa.ToDoubleAngle gap =
                               {verticalLeg = sa.verticalLeg;
                                horizontalLeg = sa.horizontalLeg;
                                thickness = sa.thickness;
                                radius = sa.radius;
                                gap = gap
                                }

    and CC_DoubleAngle = {verticalLeg : float<inch>;
                           horizontalLeg : float<inch>;
                           thickness : float<inch>;
                           radius : float<inch>;
                           gap : float<inch>}

                           interface IStructuralShape
             
                           member ccda.ToSingleAngle() : CC_SingleAngle =
                                   {verticalLeg = ccda.verticalLeg;
                                    horizontalLeg = ccda.horizontalLeg;
                                    thickness = ccda.thickness;
                                    radius = ccda.radius}

                           member ccda.Blank = ccda.ToSingleAngle().Blank
                           
                           static member EqualLegFromBlank (blank: Plate) radius gap =
                               let singleAngle = CC_SingleAngle.EqualLegFromBlank blank radius
                               singleAngle.ToDoubleAngle gap

                           static member UnequalLegFromBlankAndHLeg (blank: Plate) radius hLeg gap =
                               let singleAngle = CC_SingleAngle.UnequalLegFromBlankAndHLeg blank radius hLeg
                               singleAngle.ToDoubleAngle gap

                           static member UnequalLegFromBlankAndVLeg (blank: Plate) radius vLeg gap =
                               let singleAngle = CC_SingleAngle.UnequalLegFromBlankAndVLeg blank radius vLeg
                               singleAngle.ToDoubleAngle gap                          


    type StructuralShape =
            | Plate of Plate
            | SingleAngle of SingleAngle
            | DoubleAngle of DoubleAngle
            | CC_SingleAngle of CC_SingleAngle
            | CC_DoubleAngle of CC_DoubleAngle

            


    let description ss =
        match ss with
            | Plate pl -> String.Format("PL{0}x{1}",
                              pl.length, pl.thickness)
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

    let rec area ss =
            match ss with
            | Plate pl -> pl.length * pl.thickness
            | SingleAngle sa -> (sa.horizontalLeg + sa.verticalLeg - sa.thickness) * sa.thickness
            | DoubleAngle da -> 2.0 * (da.horizontalLeg + da.verticalLeg - da.thickness) * da.thickness
            | CC_SingleAngle ccsa -> area (Plate ccsa.Blank)
            | CC_DoubleAngle ccda -> 2.0 * area (Plate ccda.Blank)

    let xbar ss =
            match ss with
            | Plate pl -> pl.thickness/2.0
            | SingleAngle sa->
                (sa.horizontalLeg * sa.thickness * sa.horizontalLeg/2.0 +
                 ((sa.verticalLeg - sa.thickness) * sa.thickness * sa.thickness/2.0)) / (area ss)
            | DoubleAngle _ -> 0.0<inch>
            | CC_DoubleAngle _ -> 0.0<inch>

    let ybar ss =
            match ss with
            | Plate pl -> pl.length/2.0
            | SingleAngle sa ->
                ((sa.horizontalLeg - sa.thickness) * sa.thickness * sa.thickness/2.0 +
                 (sa.verticalLeg * sa.thickness * sa.verticalLeg/2.0)) / (area ss)
            | DoubleAngle da ->
                ((da.horizontalLeg - da.thickness) * da.thickness * da.thickness/2.0 +
                 (da.verticalLeg * da.thickness * da.verticalLeg/2.0)) / (area ss / 2.0)

    let getShape<'T when 'T :> IStructuralShape> ss =
        let boxedShape = 
            match ss with
            | Plate pl  -> box pl
            | SingleAngle sa -> box sa
            | DoubleAngle da -> box da
            | CC_SingleAngle ccsa -> box ccsa
            | CC_DoubleAngle ccda -> box ccda
        boxedShape :?> 'T

    
    let tryGetShape<'T when 'T :> IStructuralShape> ss =
        let boxedShape = 
            match ss with
            | Plate pl  -> box pl
            | SingleAngle sa -> box sa
            | DoubleAngle da -> box da
            | CC_SingleAngle ccsa -> box ccsa
            | CC_DoubleAngle ccda -> box ccda
        try
            Some( boxedShape :?> 'T)
        with
        | _ -> None


    type StructuralShape with
        member ss.Description = description ss
        member ss.Area = area ss
        member ss.Xbar = xbar ss
        member ss.Yar = ybar ss

    let myPlate = Plate {length = 3.0<inch>;
                         thickness = 0.25<inch>}

    let test = tryGetShape<SingleAngle> myPlate

   

    
        



    




    
    



    
    

    

