namespace Structural

open System

[<Measure>] type inch

module Shapes =

    type IShape = interface end

    type SingleAngle =
        {verticalLeg : float<inch>;
         horizontalLeg : float<inch>;
         thickness : float<inch>}
        interface IShape

    type DoubleAngle =
        {verticalLeg : float<inch>;
         horizontalLeg : float<inch>;
         thickness : float<inch>;
         gap : float<inch>}
        interface IShape
    
    type Plate =
        {length : float<inch>;
         thickness: float<inch>; }
        interface IShape
                                  
    type CC_SingleAngle =
        {verticalLeg : float<inch>;
         horizontalLeg : float<inch>;
         thickness : float<inch>;
         radius : float<inch>;}
        interface IShape

        member ccsa.Blank = 
            {length =
                 ((ccsa.horizontalLeg - ccsa.radius - ccsa.thickness) +
                  (ccsa.verticalLeg - ccsa.radius - ccsa.thickness) +
                  (2.0 * Math.PI * (ccsa.radius + ccsa.thickness/2.0) * 0.25));

             thickness = ccsa.thickness;}

    type CC_DoubleAngle = 
        {verticalLeg : float<inch>;
         horizontalLeg : float<inch>;
         thickness : float<inch>;
         radius : float<inch>;
         gap : float<inch>}
        interface IShape

        member ccda.Blank =
            {length =
                 ((ccda.horizontalLeg - ccda.radius - ccda.thickness) +
                  (ccda.verticalLeg - ccda.radius - ccda.thickness) +
                  (2.0 * Math.PI * (ccda.radius + ccda.thickness/2.0) * 0.25));

             thickness = ccda.thickness;}                      


    type Shape =
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

    let getShape<'T when 'T :> IShape> ss =
        let boxedShape = 
            match ss with
            | Plate pl  -> box pl
            | SingleAngle sa -> box sa
            | DoubleAngle da -> box da
            | CC_SingleAngle ccsa -> box ccsa
            | CC_DoubleAngle ccda -> box ccda
        boxedShape :?> 'T

    
    let tryGetShape<'T when 'T :> IShape> ss =
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



    type Shape with
        member ss.Description = description ss
        member ss.Area = area ss
        member ss.Xbar = xbar ss
        member ss.Ybar = ybar ss
        member ss.Shape= getShape ss


module ShapeOps =
    open Shapes

    module SingleAngle =
        
        let create vLeg hLeg t =
                SingleAngle {verticalLeg = vLeg;
                             horizontalLeg = hLeg;
                             thickness = t;}

    module DoubleAngle =
        
        let create vLeg hLeg t gap =
            DoubleAngle {verticalLeg = vLeg;
                         horizontalLeg = hLeg;
                         thickness = t;
                         gap = gap;}

    module Plate =
    
        let create length t =
            Plate {length = length;
                   thickness = t;}

    module CC_SingleAngle =

        let create vLeg hLeg t r =
            CC_SingleAngle {verticalLeg = vLeg;
                            horizontalLeg = hLeg;
                            thickness = t;
                            radius = r;}

        let createEqualLegFromBlank (blank: Plate) radius =
            let radius' = radius + blank.thickness/2.0
            let curveLength = 2.0 * Math.PI * radius' * 0.25
            let legLength = (blank.length - curveLength) / 2.0
            let singleAngle : CC_SingleAngle = 
                {verticalLeg = legLength;
                 horizontalLeg = legLength;
                 thickness = blank.thickness;
                 radius = radius;}
            singleAngle

        let createUnequalLegFromBlankAndHLeg (blank: Plate) radius hLeg =
            let radius' = radius + blank.thickness/2.0
            let curveLength = 2.0 * Math.PI * radius' * 0.25
            let vLeg = blank.length - curveLength - hLeg
            let singleAngle : CC_SingleAngle =
                {verticalLeg = vLeg;
                 horizontalLeg = hLeg;
                 thickness = blank.thickness;
                 radius = radius;}
            singleAngle

        let createUnequalLegFromBlankAndVLeg (blank: Plate) radius vLeg =
            let radius' = radius + blank.thickness/2.0
            let curveLength = 2.0 * Math.PI * radius' * 0.25
            let hLeg = blank.length - curveLength - vLeg
            let singleAngle : CC_SingleAngle =
                {verticalLeg = vLeg;
                 horizontalLeg = hLeg;
                 thickness = blank.thickness;
                 radius = radius;}
            singleAngle

        let toCC_DoubleAngle gap (ccsa : CC_SingleAngle)= 
            CC_DoubleAngle {verticalLeg = ccsa.verticalLeg;
                            horizontalLeg = ccsa.horizontalLeg;
                            thickness = ccsa.thickness;
                            radius = ccsa.radius;
                            gap = gap
                            }

    type Shapes.CC_SingleAngle with
        member ccsa.ToCC_DoubleAngle gap = ccsa |> CC_SingleAngle.toCC_DoubleAngle gap

    module CC_DoubleAngle =

        let create vLeg hLeg t r gap =
            CC_DoubleAngle {verticalLeg = vLeg;
                            horizontalLeg = hLeg;
                            thickness = t;
                            radius = r;
                            gap = gap;}

        let createEqualLegFromBlank (blank: Plate) radius gap =
            let singleAngle = CC_SingleAngle.createEqualLegFromBlank blank radius
            singleAngle |> CC_SingleAngle.toCC_DoubleAngle gap

        let createUnequalLegFromBlankAndHLeg (blank: Plate) radius hLeg gap =
            let singleAngle = CC_SingleAngle.createUnequalLegFromBlankAndHLeg blank radius hLeg
            singleAngle |> CC_SingleAngle.toCC_DoubleAngle gap

        let createUnequalLegFromBlankAndVLeg (blank: Plate) radius vLeg gap =
            let singleAngle = CC_SingleAngle.createUnequalLegFromBlankAndVLeg blank radius vLeg
            singleAngle |> CC_SingleAngle.toCC_DoubleAngle gap

        let toCC_SingleAngle  (ccda : CC_DoubleAngle) =
                CC_SingleAngle {verticalLeg = ccda.verticalLeg;
                                horizontalLeg = ccda.horizontalLeg;
                                thickness = ccda.thickness;
                                radius = ccda.radius}

    type Shapes.CC_DoubleAngle with
        member ccda.ToCC_SingleAngle() = ccda |> CC_DoubleAngle.toCC_SingleAngle



    
        



    




    
    



    
    

    

