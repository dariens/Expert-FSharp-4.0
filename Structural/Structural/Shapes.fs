namespace Structural.Sections

open System

[<Measure>] type inch
[<Measure>] type lbf
[<Measure>] type kip
[<Measure>] type psi = lbf/inch^2
[<Measure>] type ksi = kip/inch^2

type DesignProperties =
    {
    Area : float<inch ^2>
    XBar : float<inch>
    YBar : float<inch>
    }

[<AutoOpen>]
module Materials =

    type IMaterial =
        abstract member Fy : float<ksi> option
        abstract member Fu : float<ksi> option
        abstract member E : float<ksi> option

    module SteelMaterial =

        type T =
            {
            Fy : float<ksi>
            Fu : float<ksi>
            E : float<ksi>
            }
            interface IMaterial with
                member t.Fy = Some t.Fy
                member t.Fu = Some t.Fu
                member t.E = Some t.E
                

        let create Fy Fu E = {Fy = Fy; Fu = Fu; E = E}

    module WoodMaterial =
    
        type T =
            {
            E : float<ksi>
            }
            interface IMaterial with
                member t.Fy = None
                member t.Fu = None
                member t.E = Some t.E

        let create E = {E = E}

type Rotation =
    | Ninety
    | OneEighty
    | TwoSeventy
        
type Mirror =
    | Vertical
    | Horizontal

type Transformation =
    | Rotate of Rotation
    | Mirror of Mirror

type IShape =
    abstract member Description: string
    abstract member Length: float<inch> option
    abstract member Thickness: float<inch> option
    abstract member VerticalLeg: float<inch> option
    abstract member HorizontalLeg: float<inch> option
    abstract member Gap : float<inch> option
    abstract member Radius : float<inch> option
    abstract member DesignProperties : DesignProperties
    abstract member Material : IMaterial
    abstract member Transformations : Transformation list option
module Plate =


    type T =
        {
        Length : float<inch>
        Thickness : float<inch>
        Material : IMaterial
        Transformations : Transformation list option
        }
        member t.DesignProperties =
            {
            Area = -1.0<inch^2>
            XBar = -1.0<inch>
            YBar= -1.0<inch>
            }


        interface IShape with
           member this.Description = ""
           member this.Length = Some this.Length
           member this.Thickness = Some this.Thickness
           member this.VerticalLeg = None
           member this.HorizontalLeg = None
           member this.Gap = None
           member this.Radius = None
           member this.DesignProperties = this.DesignProperties
           member this.Material = this.Material
           member this.Transformations = this.Transformations



    let create length t material transformations=
        {
        Length = length
        Thickness = t
        Material = material
        Transformations = transformations
        }


module SingleAngle =
    
    type T =
        {
        VerticalLeg : float<inch>
        HorizontalLeg : float<inch>
        Thickness : float<inch>
        Material : IMaterial
        Transformations : Transformation list option
        }

        member t.DesignProperties =
            {
            Area = -1.0<inch^2>
            XBar = -1.0<inch>
            YBar= -1.0<inch>
            }
            
        interface IShape with
           member this.Description = ""
           member this.Length = None
           member this.Thickness = Some this.Thickness
           member this.VerticalLeg = Some this.VerticalLeg
           member this.HorizontalLeg = Some this.HorizontalLeg
           member this.Gap = None
           member this.Radius = None
           member this.DesignProperties = this.DesignProperties
           member this.Material = this.Material
           member this.Transformations = this.Transformations

    let create vLeg hLeg t material transformations=
        {
        VerticalLeg = vLeg
        HorizontalLeg = hLeg
        Thickness = t
        Material = material
        Transformations = transformations
        }


module DoubleAngle =
    
    type T =
        {
        VerticalLeg : float<inch>
        HorizontalLeg : float<inch>
        Thickness : float<inch>
        Gap : float<inch>
        Material : IMaterial
        Transformations : Transformation list option
        }

        member t.DesignProperties =
            {
            Area = -1.0<inch^2>
            XBar = -1.0<inch>
            YBar= -1.0<inch>
            }

        interface IShape with
           member this.Description = ""
           member this.Length = None
           member this.Thickness = Some this.Thickness
           member this.VerticalLeg = Some this.VerticalLeg
           member this.HorizontalLeg = Some this.HorizontalLeg
           member this.Gap = Some this.Gap
           member this.Radius = None
           member this.DesignProperties = this.DesignProperties
           member this.Material = this.Material
           member this.Transformations = this.Transformations

    let create vLeg hLeg t gap material transformations=
        {
        VerticalLeg = vLeg
        HorizontalLeg = hLeg
        Thickness = t
        Gap = gap
        Material = material
        Transformations = transformations
        }


module CF_SingleAngle =

    type T =
        {
        VerticalLeg : float<inch>
        HorizontalLeg : float<inch>
        Thickness : float<inch>
        Radius : float<inch>
        Material : IMaterial
        Transformations : Transformation list option
        }

        member t.DesignProperties =
            {
            Area = -1.0<inch^2>
            XBar = -1.0<inch>
            YBar= -1.0<inch>
            }

        interface IShape with
           member this.Description = ""
           member this.Length = None
           member this.Thickness = Some this.Thickness
           member this.VerticalLeg = Some this.VerticalLeg
           member this.HorizontalLeg = Some this.HorizontalLeg
           member this.Gap = None
           member this.Radius = Some this.Radius
           member this.DesignProperties = this.DesignProperties
           member this.Material = this.Material
           member this.Transformations = this.Transformations

        member cfsa.Blank : Plate.T = 
            {
            Length =
                ((cfsa.HorizontalLeg - cfsa.Radius - cfsa.Thickness) +
                    (cfsa.VerticalLeg - cfsa.Radius - cfsa.Thickness) +
                    (2.0 * Math.PI * (cfsa.Radius + cfsa.Thickness/2.0) * 0.25))
            Thickness = cfsa.Thickness
            Material = cfsa.Material
            Transformations = None
            }

    let create vLeg hLeg t r material transformations =
        {
        VerticalLeg = vLeg
        HorizontalLeg = hLeg
        Thickness = t
        Radius = r
        Material = material
        Transformations = transformations
        }

    let createEqualLegFromBlank (blank: Plate.T) radius material transformations =
        let radius' = radius + blank.Thickness/2.0
        let curveLength = 2.0 * Math.PI * radius' * 0.25
        let legLength = (blank.Length - curveLength) / 2.0
        let singleAngle = 
            {
            VerticalLeg = legLength
            HorizontalLeg = legLength
            Thickness = blank.Thickness
            Radius = radius
            Material = material
            Transformations = transformations
            }
        singleAngle

    let createUnequalLegFromBlankAndHLeg (blank: Plate.T) radius hLeg material transformations=
        let radius' = radius + blank.Thickness/2.0
        let curveLength = 2.0 * Math.PI * radius' * 0.25
        let vLeg = blank.Length - curveLength - hLeg
        let singleAngle =
            {
            VerticalLeg = vLeg
            HorizontalLeg = hLeg
            Thickness = blank.Thickness
            Radius = radius
            Material = material
            Transformations = transformations
            }
        singleAngle

    let createUnequalLegFromBlankAndVLeg (blank: Plate.T) radius vLeg material transformations=
        let radius' = radius + blank.Thickness/2.0
        let curveLength = 2.0 * Math.PI * radius' * 0.25
        let hLeg = blank.Length - curveLength - vLeg
        let singleAngle =
            {
            VerticalLeg = vLeg
            HorizontalLeg = hLeg
            Thickness = blank.Thickness
            Radius = radius
            Material = material
            Transformations = transformations
            }
        singleAngle


module CF_DoubleAngle =

    type T =
        {
        VerticalLeg : float<inch>
        HorizontalLeg : float<inch>
        Thickness : float<inch>
        Radius : float<inch>
        Gap : float<inch>
        Material : IMaterial
        Transformations : Transformation list option
        }

        member t.DesignProperties =
            {
            Area = -1.0<inch^2>
            XBar = -1.0<inch>
            YBar= -1.0<inch>
            }

        interface IShape with
           member this.Description = ""
           member this.Length = None
           member this.Thickness = Some this.Thickness
           member this.VerticalLeg = Some this.VerticalLeg
           member this.HorizontalLeg = Some this.HorizontalLeg
           member this.Gap = Some this.Gap
           member this.Radius = Some this.Radius
           member this.DesignProperties = this.DesignProperties
           member this.Material = this.Material
           member this.Transformations = this.Transformations


        member cfda.Blank : Plate.T = 
            {
            Length =
                ((cfda.HorizontalLeg - cfda.Radius - cfda.Thickness) +
                    (cfda.VerticalLeg - cfda.Radius - cfda.Thickness) +
                    (2.0 * Math.PI * (cfda.Radius + cfda.Thickness/2.0) * 0.25))
            Thickness = cfda.Thickness
            Material = cfda.Material
            Transformations = None
            }

    let create vLeg hLeg t r gap material transformations=
        {
        VerticalLeg = vLeg
        HorizontalLeg = hLeg
        Thickness = t
        Radius = r
        Gap = gap
        Material = material
        Transformations = transformations
        }
  
    let createEqualLegFromBlank (blank: Plate.T) radius gap material transformations=
        let radius' = radius + blank.Thickness/2.0
        let curveLength = 2.0 * Math.PI * radius' * 0.25
        let legLength = (blank.Length - curveLength) / 2.0
        let singleAngle = 
            {
            VerticalLeg = legLength
            HorizontalLeg = legLength
            Thickness = blank.Thickness
            Radius = radius
            Gap = gap
            Material = material
            Transformations = transformations
            }
        singleAngle

    let createUnequalLegFromBlankAndHLeg (blank: Plate.T) radius hLeg gap material transformations =
        let radius' = radius + blank.Thickness/2.0
        let curveLength = 2.0 * Math.PI * radius' * 0.25
        let vLeg = blank.Length - curveLength - hLeg
        let doubleAngle =
            {
            VerticalLeg = vLeg
            HorizontalLeg = hLeg
            Thickness = blank.Thickness
            Radius = radius
            Gap = gap
            Material = material
            Transformations = transformations
            }
        doubleAngle

    let createUnequalLegFromBlankAndVLeg (blank: Plate.T) radius vLeg gap material transformations=
        let radius' = radius + blank.Thickness/2.0
        let curveLength = 2.0 * Math.PI * radius' * 0.25
        let hLeg = blank.Length - curveLength - vLeg
        let doubleAngle =
            {
            VerticalLeg = vLeg
            HorizontalLeg = hLeg
            Thickness = blank.Thickness
            Radius = radius
            Gap = gap
            Material = material
            Transformations = transformations
            }
        doubleAngle

(*
type Shape =
    | Plate of Plate.T
    | SingleAngle of SingleAngle.T
    | DoubleAngle of DoubleAngle.T
    | CF_SingleAngle of CF_SingleAngle.T
    | CF_DoubleAngle of CF_DoubleAngle.T


module ShapeOps =

    let description shape =
        match shape with
        | Plate pl -> String.Format("PL{0}x{1}", pl.Length, pl.Thickness)
        | SingleAngle sa ->
            String.Format("L{0}x{1}x{2}", sa.VerticalLeg, sa.HorizontalLeg, sa.Thickness)
        | DoubleAngle da -> 
            String.Format("2L{0}x{1}x{2}x{3}", da.VerticalLeg,
                da.HorizontalLeg, da.Thickness, da.Gap)
        | CF_SingleAngle cfsa ->
            String.Format("CFL{0}x{1}x{2}r{3}",
                cfsa.VerticalLeg, cfsa.HorizontalLeg,
                cfsa.Thickness, cfsa.Radius)
        | CF_DoubleAngle cfda ->
            String.Format("CF2L{0}x{1}x{2}x{3}r{4}",
                cfda.VerticalLeg, cfda.HorizontalLeg,
                cfda.Thickness, cfda.Gap, cfda.Radius)

    let depth shape =
        match shape with
        | Plate pl -> pl.Length
        | SingleAngle sa -> sa.VerticalLeg
        | DoubleAngle da -> da.VerticalLeg
        | CF_SingleAngle cfsa -> cfsa.VerticalLeg
        | CF_DoubleAngle cfda -> cfda.VerticalLeg

    let width shape =
        match shape with
        | Plate pl -> pl.Thickness
        | SingleAngle sa -> sa.HorizontalLeg
        | DoubleAngle da -> da.HorizontalLeg
        | CF_SingleAngle cfsa -> cfsa.HorizontalLeg
        | CF_DoubleAngle cfda -> cfda.HorizontalLeg

    let rec area shape =
        match shape with
        | Plate pl -> pl.Length * pl.Thickness
        | SingleAngle sa ->
            (sa.HorizontalLeg + sa.VerticalLeg - sa.Thickness) * sa.Thickness
        | DoubleAngle da ->
            2.0 * (da.HorizontalLeg + da.VerticalLeg - da.Thickness) * da.Thickness
        | CF_SingleAngle cfsa -> area (Plate cfsa.Blank)
        | CF_DoubleAngle cfda -> 2.0 * (area (Plate cfda.Blank))

    let xBar shape =
        match shape with
        | Plate pl -> pl.Thickness/2.0
        | SingleAngle sa ->
            (sa.HorizontalLeg * sa.Thickness * sa.HorizontalLeg/2.0 +
             ((sa.VerticalLeg - sa.Thickness) * sa.Thickness * sa.Thickness/2.0))
             / (area (SingleAngle sa))
        | DoubleAngle da -> 0.0<inch>
        | CF_SingleAngle cfsa -> -1.0<inch>
        | CF_DoubleAngle cfda -> -1.0<inch>

    let yBar shape =
        match shape with
        | Plate pl -> pl.Length/2.0
        | SingleAngle sa ->
            ((sa.HorizontalLeg - sa.Thickness) * sa.Thickness * sa.Thickness/2.0 +
             (sa.VerticalLeg * sa.Thickness * sa.VerticalLeg/2.0))
             / (area (SingleAngle sa))
        | DoubleAngle da ->
            ((da.HorizontalLeg - da.Thickness) * da.Thickness * da.Thickness/2.0 +
             (da.VerticalLeg * da.Thickness * da.VerticalLeg/2.0))
             / ((area (DoubleAngle da)) / 2.0)
        | CF_SingleAngle cfsa -> -1.0<inch>
        | CF_DoubleAngle cfda -> -1.0<inch>


    let designProperties shape =
        {
        Area = area shape
        XBar = xBar shape
        YBar = yBar shape
        }

    type Shape with
        member s.DesignProperties = designProperties s
        member s.Descrition = s.Descrition


[<AutoOpen>]
module Shapes =
    open ShapeOps

    type Plate.T with
        member pl.Description = description (Plate pl)
        member pl.DesignProperties = designProperties (Plate pl)

    type SingleAngle.T with
        member sa.Description = description (SingleAngle sa)
        member sa.DesignProperties = designProperties (SingleAngle sa)

    type DoubleAngle.T with
        member da.Description = description (DoubleAngle da)
        member da.DesignProperties = designProperties (DoubleAngle da)

    type CF_SingleAngle.T with
        member cfsa.Description = description (CF_SingleAngle cfsa)
        member cfsa.DesignProperties = designProperties (CF_SingleAngle cfsa)

    type CF_DoubleAngle.T with
        member cfda.Description = description (CF_DoubleAngle cfda)
        member cfda.DesignProperties = designProperties (CF_DoubleAngle cfda)


*)


    


        



    




    
    



    
    

    

