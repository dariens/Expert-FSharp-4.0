open System
open System.Collections.Generic

[<Measure>] type inch
[<Measure>] type lbf
[<Measure>] type kip 
[<Measure>] type psi = lbf/inch^2
[<Measure>] type ksi = kip/inch^2


type SingleAngle = {horizontalLeg : float<inch>;
                    verticalLeg : float<inch>;
                    thickness : float<inch>;
                    radius : float<inch>}

                    static member Create hLeg vLeg t r = {horizontalLeg = hLeg;
                                                          verticalLeg = vLeg;
                                                          thickness = t;
                                                          radius = r}

type DoubleAngle = {horizontalLeg : float<inch>;
                    verticalLeg : float<inch>;
                    thickness : float<inch>;
                    radius : float<inch>;
                    gap : float<inch>}

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


type SteelMaterial = {fy : float<kip/inch^2>
                      fu : float<kip/inch^2>
                      E : float<kip/inch^2>}

type Material =
    | SteelMaterial of SteelMaterial

type Calculation<'TVal> = {Description : string;
                           Value : 'TVal;
                           Units : string
                           Reference : string}

                           override t.ToString() =
                               sprintf "%s: %A <%s> (%s)" t.Description t.Value t.Units t.Reference

type Section = {shape : StructuralShape
                material : Material}

                member t.AllowableTension =
                    match t.material with
                    | SteelMaterial sm -> {Description = "Allowable Tension"
                                           Value = (t.shape.Area * sm.fy) / 1.67;
                                           Units = "Kip"
                                           Reference = "AISC"}




