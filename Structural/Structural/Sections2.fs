namespace Structural

(*
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

type IMaterial =
    abstract member Fy : float<ksi> option
    abstract member Fu : float<ksi> option
    abstract member E : float<ksi> option

[<AutoOpen>]
module Materials =

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



[<AutoOpen>]
module Section =
    open ShapeOps

    type Section<'TShape, 'TMaterial when 'TShape :> IShape and 'TMaterial :> IMaterial> =
        {
        Shape : 'TShape
        Material : 'TMaterial
        Transformations : seq<Transformation> option
        }

    let create shape material transformations =
        {
        Shape = shape
        Material = material
        Transformations = transformations
        }

*)