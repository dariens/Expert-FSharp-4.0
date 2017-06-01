open System
open System.Collections.Generic

type SingleAngle = {horizontalLeg : float;
                    verticalLeg : float;
                    thickness : float;
                    radius : float}

                    static member Create hLeg vLeg t r = {horizontalLeg = hLeg;
                                                          verticalLeg = vLeg;
                                                          thickness = t;
                                                          radius = r}

type DoubleAngle = {horizontalLeg : float;
                    verticalLeg : float;
                    thickness : float;
                    radius : float;
                    gap : float}

                    static member Create hLeg vLeg t r gap = {horizontalLeg = hLeg;
                                                              verticalLeg = vLeg;
                                                              thickness = t;
                                                              radius = r;
                                                              gap = gap}
                                                              
type StructuralShape =
    | SingleAngle of SingleAngle
    | DoubleAngle of DoubleAngle

    member ss.Area =
        match ss with
        | SingleAngle sa -> (sa.horizontalLeg + sa.verticalLeg - sa.thickness) * sa.thickness
        | DoubleAngle da -> 2.0 * (da.horizontalLeg + da.verticalLeg - da.thickness) * da.thickness

    member ss.Description =
        match ss with
        | SingleAngle sa -> String.Format("L{0}x{1}x{2}",
                                          sa.horizontalLeg, sa.verticalLeg, sa.thickness)
        | DoubleAngle da -> String.Format("2L{0}x{1}x{2}",
                                          da.horizontalLeg, da.verticalLeg, da.thickness)

type SteelMaterial = {fy : float
                      fu : float}

                      static member ASTM = {fy = 50000.0; fu = 60000.0}

type Material =
    | SteelMaterial of SteelMaterial


type Section = {shape : StructuralShape
                material : Material}

                member t.AllowableTension =
                    match t.material with
                    | SteelMaterial sm -> (t.shape.Area * sm.fy) / 1.67




module Testing =
    let myAngle = SingleAngle {horizontalLeg = 3.0
                               verticalLeg = 3.0
                               thickness = 0.25;
                               radius = 0.0}

    let myDoubleAngle = DoubleAngle{horizontalLeg = 3.0
                                    verticalLeg = 3.0
                                    thickness = 0.25
                                    radius = 0.0
                                    gap = 1.0}


    printfn @"%s Area : %f
    %s Area : %f" myAngle.Description myAngle.Area myDoubleAngle.Description myDoubleAngle.Area

    type ASTM =
        static member ASTM1 = SteelMaterial {fy = 50000.0; fu = 60000.0}
        static member ASTM2 = SteelMaterial {fy = 33000.0; fu = 60000.0}



    let createAngle hLeg vLeg t r = SingleAngle {horizontalLeg = hLeg;
                                                 verticalLeg = vLeg;
                                                 thickness = t;
                                                 radius = r}
    type AISC =
        static member ``L2x2x1/4`` = SingleAngle (SingleAngle.Create 2.0 2.0 0.25 0.0)
        static member ``L3x3x1/4`` = SingleAngle (SingleAngle.Create 3.0 3.0 0.25 0.0)

        
  
    let myTwobyTwobyQuarter = AISC.``L2x2x1/4``

    let test = ASTM.ASTM1

    let myMaterial = SteelMaterial {fy = 50000.0; fu = 60000.0}

    let mySection = {shape = myAngle
                     material = ASTM.ASTM1}

    printfn "%s Allowable Tension = %f" mySection.shape.Description mySection.AllowableTension



